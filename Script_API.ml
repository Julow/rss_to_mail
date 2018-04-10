module XmlService =
struct

	class type namespace =
	object
	end

	class type attribute =
	object
		method getValue : Js.js_string Js.t Js.meth
	end

	class type element =
	object
		method getChild : Js.js_string Js.t -> element Js.t Js.opt Js.meth
		method getChild_ns : Js.js_string Js.t -> namespace Js.t -> element Js.t Js.opt Js.meth
		method getChildren : Js.js_string Js.t -> element Js.t Js.js_array Js.t Js.meth
		method getChildren_tag : Js.js_string Js.t -> element Js.t Js.js_array Js.t Js.meth
		method getChildren_ns : Js.js_string Js.t -> namespace Js.t -> element Js.t Js.js_array Js.t Js.meth
		method getName : Js.js_string Js.t Js.meth
		method getText : Js.js_string Js.t Js.meth
		method getChildText : Js.js_string Js.t -> Js.js_string Js.t Js.opt Js.meth
		method getChildText_ns : Js.js_string Js.t -> namespace Js.t -> Js.js_string Js.t Js.opt Js.meth
		method getAttribute : Js.js_string Js.t -> attribute Js.t Js.opt Js.meth
		method getAttribute_ns : Js.js_string Js.t -> namespace Js.t -> attribute Js.t Js.opt Js.meth
		method addContent : element Js.t -> element Js.t Js.meth
		method setText : Js.js_string Js.t -> element Js.t Js.meth
		method setAttribute : Js.js_string Js.t -> Js.js_string Js.t -> element Js.t Js.meth
	end

	class type document =
	object
		method getRootElement : element Js.t Js.opt Js.meth
	end

	class type format =
	object
		method format : document Js.t -> Js.js_string Js.t Js.meth
		method format_element : element Js.t -> Js.js_string Js.t Js.meth
	end

	class type t =
	object
		method createDocument : element Js.t -> document Js.t Js.meth
		method createElement : Js.js_string Js.t -> element Js.t Js.meth
		method createElement_ns : Js.js_string Js.t -> namespace Js.t -> element Js.t Js.meth
		method getNamespace : Js.js_string Js.t -> namespace Js.t Js.meth
		method parse : Js.js_string Js.t -> document Js.t Js.meth
		method getPrettyFormat : format Js.t Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "XmlService"

end

module SpreadsheetApp =
struct

	class type range =
	object
		method getValues : < toString : Js.js_string Js.t Js.meth > Js.t Js.js_array Js.t Js.js_array Js.t Js.meth
	end

	class type sheet =
	object
		method getLastRow : int Js.meth
		method getRange : int -> int -> int -> int -> range Js.t Js.meth
		method appendRow : Js.js_string Js.t Js.js_array Js.t -> unit Js.meth
	end

	class type spreadsheet =
	object
		method getSheets : sheet Js.t Js.js_array Js.t Js.meth
		method getId : Js.js_string Js.t Js.meth
	end

	class type t =
	object
		method openById : Js.js_string Js.t -> spreadsheet Js.t Js.meth
		method create : Js.js_string Js.t -> int -> int -> spreadsheet Js.t Js.meth
		method flush : unit Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "SpreadsheetApp"

end

module UrlFetchApp =
struct

	class type http_response =
	object
		method getContentText : Js.js_string Js.t Js.meth
		method getResponseCode : int Js.meth
	end

	class type t =
	object
		method fetchAll : < .. > Js.t Js.js_array Js.t -> http_response Js.t Js.js_array Js.t Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "UrlFetchApp"

end

module CacheService =
struct

	class type cache =
	object
		method get : Js.js_string Js.t -> Js.js_string Js.t Js.opt Js.meth
		method getAll : Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Jstable.t Js.meth
		method put : Js.js_string Js.t -> Js.js_string Js.t -> int -> unit Js.meth
	end

	class type t =
	object
		method getScriptCache : cache Js.t Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "CacheService"

end

module PropertiesService =
struct

	class type properties =
	object
		method getProperty : Js.js_string Js.t -> Js.js_string Js.t Js.opt Js.meth
		method setProperty : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
	end

	class type t =
	object
		method getUserProperties : properties Js.t Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "PropertiesService"

end

module ContentService =
struct

	module MimeType =
	struct

		class type t = object end

		let _ATOM : t Js.t = Js.Unsafe.js_expr "ContentService.MimeType.ATOM"

	end

	class type mime_type = object end

	class type text_output =
	object
		method setMimeType : MimeType.t Js.t -> text_output Js.t Js.meth
	end

	class type t =
	object
		method createTextOutput : Js.js_string Js.t -> text_output Js.t Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "ContentService"

end

module Console =
struct

	class type t =
	object
		method info : Js.js_string Js.t -> unit Js.meth
		method error : Js.js_string Js.t -> unit Js.meth
		method time : Js.js_string Js.t -> unit Js.meth
		method timeEnd : Js.js_string Js.t -> unit Js.meth
	end

	let t : t Js.t = Js.Unsafe.js_expr "console"

	let info str = t##info (Js.string str)
	let error str = t##info (Js.string str)

end
