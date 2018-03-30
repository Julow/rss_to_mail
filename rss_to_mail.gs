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
		return JSON.parse(cached);
	var data = fetch_feed(url);
	Cache.put(id, JSON.stringify(data), cache_time);
	return data;
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

// Process feeds
// =============

// Similar to Object.assign({}, defaults, obj) but modify `obj` in-place
function obj_default(defaults, obj)
{
	for (var prop in defaults)
	{
		if (defaults.hasOwnProperty(prop)
			&& !obj.hasOwnProperty(prop))
			obj[prop] = defaults[prop];
	}
}

// Returns the list of new entries
// Entries are sorted by date, oldest first
function new_entries(last_update, entries)
{
	entries = entries.sort(function(a, b) { return b.date - a.date; });
	var i = entries.length;
	while (i > 0 && last_update < entries[i - 1].date)
		i--;
	return entries.slice(i, entries.length);
}

var CACHE_BASE_TIME = 60*30;

function update_feed(feed_states, user_email, feed_url, feed_options)
{
	var feed = cached_fetch(feed_url, feed_options.cache * CACHE_BASE_TIME);
	// For feeds without history, returns the 5 last items
	var entries = !feed_states.last_update.hasOwnProperty(feed_url)
		? new_entries(0, feed.entries).slice(-5)
		: new_entries(feed_states.last_update[feed_url], feed.entries);
	entries.forEach(function(entry)
	{
		var [ subject, body ] = email_format(feed.title, feed.link, entry);
		Logger.log("New entry in " + feed_url + ": " + entry.link);
		MailApp.sendEmail(user_email, subject, body);
		feed_states.last_update[feed_url] = entry.date;
	});
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
	if (last_row <= 1)
		return [];
	return sheet.getRange(2, 1, last_row - 1, 2)
		.getValues().map(function(row)
		{
			var url = row[0];
			var options = JSON.parse(row[1]);
			obj_default(default_options, options);
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

// Main
// ====

function update()
{
	var user_email = Session.getActiveUser().getEmail();
	var properties = PropertiesService.getUserProperties();
	var feed_states;
	try
	{
		var data = properties.getProperty("FEED_STATES");
		if (data == null) throw "Not set";
		feed_states = JSON.parse(data);
	}
	catch (exn)
	{
		Logger.log("No feed states (" + exn + ")");
		feed_states = {};
	}
	obj_default({
		sheet_id: null,
		last_update: {}
	}, feed_states);
	try
	{
		if (feed_states.sheet_id == null)
		{
			// First use, just create the spreadsheet
			feed_states.sheet_id = create_spreadsheet();
			Logger.log("Created spreadsheet");
		}
		else
		{
			read_spreadsheet(feed_states.sheet_id)
				.forEach(function(feed)
				{
					try { update_feed(feed_states, user_email, feed[0], feed[1]); }
					catch (exn) { throw feed[0] + ": " + exn; }
				});
		}
	}
	finally {
		properties.setProperty("FEED_STATES", JSON.stringify(feed_states));
	}
}

function clear_props()
{
	PropertiesService.getUserProperties().deleteAllProperties();
}

function main()
{
	ScriptApp.newTrigger("update")
		.timeBased()
		.everyMinutes(45)
		.create();
}
