// RSS feed to inbox
// Error handling is done by throwing strings

// Retrieving feeds
// ================

// Parse atom feeds
function parse_atom(feed_elem)
{
	var ns = XmlService.getNamespace('http://www.w3.org/2005/Atom');
	function get_alternate_link(feed_elem)
	{
		function is_alternate(link)
		{
			return link.getAttribute("rel").getValue() == "alternate";
		}
		var links = feed_elem.getChildren("link", ns).filter(is_alternate);
		if (links.length == 0)
			return null;
		return links[0].getAttribute("href").getValue();
	}
	function parse_entry(entry)
	{
		function parse_category(cat)
		{
			return {
				term: cat.getAttribute("term").getValue(),
				label: cat.getAttribute("label").getValue()
			};
		}
		return {
			author: entry.getChild("author", ns).getChildText("name", ns),
			categories: entry.getChildren("category", ns).map(parse_category),
			title: entry.getChildText("title", ns),
			content: entry.getChildText("content", ns),
			id: entry.getChildText("id", ns),
			link: entry.getChild("link", ns).getAttribute("href").getValue(),
			date: new Date(entry.getChildText("updated", ns)).getTime()
		};
	}
	return {
		title: feed_elem.getChildText("title", ns),
		link: get_alternate_link(feed_elem),
		entries: feed_elem.getChildren("entry", ns).map(parse_entry)
	};
}

// Parse RSS feeds
function parse_rss(rss_elem)
{
	var dc_ns = XmlService.getNamespace("http://purl.org/dc/elements/1.1/");
	var channel_elem = rss_elem.getChild("channel");
	function parse_item(item)
	{
		function parse_category(cat)
		{
			return { term: null, label: cat.getText() };
		}
		return {
			author: item.getChildText("creator", dc_ns),
			categories: item.getChildren("category").map(parse_category),
			title: item.getChildText("title"),
			content: item.getChildText("description"),
			id: item.getChildText("guid"),
			link: item.getChildText("link"),
			date: new Date(item.getChildText("pubDate")).getTime()
		};
	}
	return {
		title: channel_elem.getChildText("title"),
		link: channel_elem.getChildText("link"),
		entries: channel_elem.getChildren("item").map(parse_item)
	};
}

// Parse an Atom/RSS feed
// May throw some error if the feed is malformed
function parse_feed(contents)
{
	try
	{
		var doc = XmlService.parse(contents);
		var root_elem = doc.getRootElement();
		switch (root_elem.getName())
		{
			case "rss": return parse_rss(root_elem);
			case "feed": return parse_atom(root_elem);
			default: throw "Unexpected format";
		}
	}
	catch (exn)
	{
		throw "Parsing failed: " + exn;
	}
}

// Fetch and parse a feed
// If the fetch failed, throw an exception
function fetch_feed(url)
{
	var re = UrlFetchApp.fetch(url);
	var code = re.getResponseCode();
	if (code == 200)
		return parse_feed(re.getContentText());
	else
		throw "Fetch failed: " + code;
}

var Cache = CacheService.getScriptCache();

// Cached call to fetch_feed
// `cache_time` is in second
function cached_fetch(url, cache_time)
{
	var id = Utilities.base64Encode(url);
	var cached = Cache.get(id);
	if (cached)
		return cached;
	var data = fetch_feed(url);
	Cache.put(id, data, cache_time);
	return data;
}

// Process feeds
// =============

// Returns the list of new entries
// Update the feed state
// Entries are sorted by date, newest first
// Returns 5 entries for feeds without history
function new_entries(feed_states, id, entries)
{
	entries = entries.sort(function(a, b) { return a.date - b.date; });
	if (entries.length == 0)
		return [];
	var i = 0;
	if (!feed_states.hasOwnProperty(id))
	{
		feed_states[id] = {};
		i = 5;
	}
	else
	{
		var newer_than = feed_states[id].last_update;
		while (i < entries.length && newer_than < entries[i].date)
			i++;
	}
	feed_states[id].last_update = entries[0].date;
	return entries.slice(0, i);
}

// Sending mails
// =============

function email_format(feed_title, feed_url, entry)
{
	var subject = feed_title + ": " + entry.title;
	var categories = entry.categories.map(function(c){return c.label;}).join(" ");
	var date = new Date(entry.date).toGMTString();
	var body = "Via <a href=\"" + feed_url + "\">" + feed_title + "</a> (" + categories + ")\n"
		+ "on " + date + " by " + entry.author + "\n"
		+ "<a href=\"" + entry.link + "\">" + entry.title + "</a>\n"
		+ "\n"
		+ entry.content;
	return [ subject, body ];
}

// Feed list
// =========
// Feeds are stored in a Spreadsheet in two columns: feed url, options
// Options use the JSON format

var default_options = {
	"cache": 1
};

// Load feeds stored in a Spreadsheet
function read_spreadsheet(sheet_id)
{
	var ss = SpreadsheetApp.openById(sheet_id);
	var sheet = ss.getSheets()[0];
	var last_row = ss.getLastRow();
	return sheet.getRange(2, 1, last_row - 1, 2)
		.getValues().map(function(row)
		{
			var url = row[0];
			var options = JSON.parse(row[1]);
			for (var opt in default_options)
			{
				if (!options.hasOwnProperty(opt))
					options[opt] = default_options[opt];
			}
			return [ url, options ];
		});
}

// Create an empty spreadsheet
function create_spreadsheet()
{
	var ss = SpreadsheetApp.create("Rss to mail", 1, 2);
	var sheet = ss.getSheets()[0];
	sheet.appendRow([ "Feed url", "Options" ]);
	SpreadsheetApp.flush();
	return ss.getId();
}

function load_feeds(feed_states)
{
	if (!feed_states.hasOwnProperty("sheet_id"))
	{
		feed_states.sheet_id = create_spreadsheet();
		return [];
	}
	else
	{
		return read_spreadsheet(feed_states.sheet_id);
	}
}

// Main
// ====

var Properties = PropertiesService.getUserProperties();

// Calls `f` with the value of the property `name`
// The value is expected to be an object, the default value is `{}`
// If it is modified, the property is set back
function with_property(name, f)
{
	var string_data = Properties.getProperty(name);
	var data = (string_data === null) ? {} : JSON.parse(string_data);
	f(data);
	var data_string = JSON.stringify(data);
	if (data_string != string_data)
		Properties.setProperty(name, data_string);
}

// Configs
var CACHE_BASE_TIME = 60*30;

function update()
{
	var user_email = Session.getActiveUser().getEmail();
	with_property("FEED_STATES", function(feed_states)
	{
		load_feeds(feed_states)
			.forEach(function([ url, options ])
		{
			var feed = cached_fetch(url, options.cache * CACHE_BASE_TIME);
			new_entries(feed_states, url, feed.entries).forEach(function(entry)
			{
				var [ subject, body ] = email_format(feed.title, feed.link, entry);
				Logger.log("New entry in " + url + ": " + entry.link);
				MailApp.sendEmail(user_email, subject, body);
			});
		});
	});
}

function clear_props()
{
	Properties.deleteAllProperties();
}

function main()
{
	ScriptApp.newTrigger("update")
		.timeBased()
		.everyMinutes(45)
		.create();
}
