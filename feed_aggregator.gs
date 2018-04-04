// Feed aggregator
// Error handling is done by throwing strings

// Retrieving feeds
// ================

var ATOM_NAMESPACE = XmlService.getNamespace('http://www.w3.org/2005/Atom');

// Parse atom feeds
function parse_atom(feed_elem)
{
	var ns = ATOM_NAMESPACE;
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
				label: cat.getAttribute("label").getValue().toString()
			};
		}
		return {
			author: entry.getChild("author", ns).getChildText("name", ns).toString(),
			categories: entry.getChildren("category", ns).map(parse_category),
			title: entry.getChildText("title", ns).toString(),
			content: entry.getChildText("content", ns).toString(),
			id: entry.getChildText("id", ns).toString(),
			link: entry.getChild("link", ns).getAttribute("href").getValue().toString(),
			date: new Date(entry.getChildText("updated", ns)).getTime()
		};
	}
	return {
		title: feed_elem.getChildText("title", ns).toString(),
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
			return { term: null, label: cat.getText().toString() };
		}
		return {
			author: item.getChildText("creator", dc_ns).toString(),
			categories: item.getChildren("category").map(parse_category),
			title: item.getChildText("title").toString(),
			content: item.getChildText("description").toString(),
			id: item.getChildText("guid").toString(),
			link: item.getChildText("link").toString(),
			date: new Date(item.getChildText("pubDate")).getTime()
		};
	}
	return {
		title: channel_elem.getChildText("title").toString(),
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
	Logger.log("Fetched " + data.entries.length + " entries from " + url);
	Cache.put(id, JSON.stringify(data), cache_time);
	return data;
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

// Comparaison function (to use with [].sort())
// Sort by date, newest first
function entries_by_date(a, b)
{
	return b.date - a.date;
}

// Returns the list of new entries, sorted
function new_entries(last_update, entries)
{
	entries = entries.sort(entries_by_date);
	var i = 0;
	while (i < entries.length && entries[i].date > last_update)
		i++;
	return entries.slice(0, i);
}

function entry_content(feed, feed_options, entry)
{
	var categories = entry.categories.map(function(c){return c.label;}).join(" ");
	var date = new Date(entry.date).toGMTString();
	return "Via <a href=\"" + feed.link + "\">" + feed.title + "</a> (" + categories + ")<br/>"
		+ "on " + date + " by " + entry.author + "<br/>"
		+ "<a href=\"" + entry.link + "\">" + entry.title + "</a>\n"
		+ "\n"
		+ entry.content;
}

function extract_entries(feed, feed_options, since)
{
	function update_entry(entry)
	{
		var feed_name = feed_options.name || feed.title;
		var updated = {
			id: feed.link + entry.id,
			title: feed_name + ": " + entry.title,
			content: entry_content(feed, feed_options, entry)
		};
		obj_default(entry, updated);
		return updated;
	}
	return new_entries(since, feed.entries).map(update_entry);
}

// Feed list
// =========
// Feeds are stored in a Spreadsheet in two columns: feed url, options
// Options use the JSON format

// Load feeds stored in a Spreadsheet
function read_spreadsheet(sheet_id, default_options)
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
			var options;
			try
			{
				options = JSON.parse(row[1]);
			}
			catch (exn)
			{
				Logger.log("Malformed options (" + url + "): " + exn);
				options = {};
			}
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

// Output feed
// ===========

function new_elem(name)
{
	return XmlService.createElement(name, ATOM_NAMESPACE);
}

function format_entry(entry)
{
	var node = new_elem("entry")
	node.addContent(new_elem("author")
			.addContent(new_elem("name")
				.setText(entry.author)));
	node.addContent(new_elem("title")
			.setText(entry.title));
	node.addContent(new_elem("content")
			.setAttribute("type", "html")
			.setText(entry.content));
	node.addContent(new_elem("id")
			.setText(entry.id));
	node.addContent(new_elem("link")
			.setAttribute("href", entry.link));
	node.addContent(new_elem("updated")
			.setText(new Date(entry.date).toISOString()));
	entry.categories.forEach(function(cat)
	{
		node.addContent(new_elem("category")
				.setAttribute("label", cat.label)
				.setAttribute("term", cat.term || ""));
	});
	return node;
}

function format_atom(entries)
{
	var root = new_elem("feed")
			.addContent(new_elem("title").setText("Feed aggregator"));
	entries.forEach(function(entry)
	{
		root.addContent(format_entry(entry));
	});
	var doc = XmlService.createDocument(root);
	return XmlService.getPrettyFormat().format(doc);
}

// Main
// ====

// in seconds
var CACHE_BASE_TIME = 60*30;

// in milliseconds
var OLDEST_ENTRY = 1000*60*60*24*7;

// Supported options and their default value
var DEFAULT_OPTIONS = {
	"cache": 1,
	"name": null
};

var Properties = PropertiesService.getUserProperties();

/*
** Fetch the content of the spreadsheet
** If it does not exits, it is created
** Store the sheet id in the PropertiesService (UserProperties)
*/
function load_spreadsheet(default_options)
{
	var sheet_id = JSON.parse(Properties.getProperty("SHEET_ID"));
	if (sheet_id === null)
	{
		var sheet_id = create_spreadsheet();
		Properties.setProperty("SHEET_ID", JSON.stringify(sheet_id));
		return [];
	}
	else
	{
		return read_spreadsheet(sheet_id, default_options);
	}
}

function doGet()
{
	var feed_entries = load_spreadsheet(DEFAULT_OPTIONS).map(function(feed)
	{
		var feed_url = feed[0];
		var feed_options = feed[1];
		try
		{
			var feed = cached_fetch(feed_url, feed_options.cache * CACHE_BASE_TIME);
			var since = new Date().getTime() - OLDEST_ENTRY;
			return extract_entries(feed, feed_options, since);
		}
		catch (exn)
		{
			Logger.log("Error while processing feed " + feed_url + ": " + exn);
			return [];
		}
	});
	var entries = [].concat.apply([], feed_entries).sort(entries_by_date);
	Logger.log(entries.length + " entries");
	return ContentService.createTextOutput(format_atom(entries))
		.setMimeType(ContentService.MimeType.ATOM);
}
