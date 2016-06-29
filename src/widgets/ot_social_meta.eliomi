[%%server.start]

type t =

  | Title                       (* <title> tag *)
  | Description                 (* description *)

  (* https://developers.facebook.com/docs/sharing/webmasters#markup *)
  | Og_title                    (* og:title *)
  | Og_site_name                (* og:site_name *)
  | Og_type                     (* og:type *)
  | Og_description              (* og:description *)
  | Og_locale                   (* og:locale *)
  | Og_locale_alternate         (* og:locale:alternate *)
  | Og_video                    (* og:video *)
  | Og_video_url                (* og:video:url *)
  | Og_video_secure_url         (* og:video:secure_url *)
  | Og_video_type               (* og:video:type *)
  | Og_video_with               (* og:video:with *)
  | Og_video_height             (* og:video:height *)
  | Og_image                    (* og:image *)
  | Og_url                      (* og:url *)
  | Og_image_secure_url         (* og:image:secure_url *)
  | Og_image_type               (* og:image:type *)
  | Og_image_with               (* og:image:with *)
  | Og_image_height             (* og:image:height *)

  | Fb_admins                   (* fb:admins *)

  (* https://dev.twitter.com/cards/markup *)
  | Twitter_card                (* twitter:card *)
  | Twitter_site                (* twitter:site *)
  | Twitter_site_id             (* twitter:site:id *)
  | Twitter_creator             (* twitter:creator *)
  | Twitter_creator_id          (* twitter:creator:id *)
  | Twitter_url                 (* twitter:url  *)
  | Twitter_title               (* twitter:title *)
  | Twitter_description         (* twitter:description *)
  | Twitter_image               (* twitter:image *)
  | Twitter_image_alt           (* twitter:image:alt *)
  | Twitter_player              (* twitter:player *)
  | Twitter_player_width        (* twitter:player:width *)
  | Twitter_player_height       (* twitter:player:height *)
  | Twitter_player_stream       (* twitter:player:stream *)
  | Twitter_app_name_iphone     (* twitter:app:name:iphone *)
  | Twitter_app_id_iphone       (* twitter:app:id:iphone *)
  | Twitter_app_url_iphone      (* twitter:app:url:iphone *)
  | Twitter_app_name_ipad       (* twitter:app:name:ipad *)
  | Twitter_app_id_ipad         (* twitter:app:id:ipad *)
  | Twitter_app_url_ipad        (* twitter:app:url:ipad *)
  | Twitter_app_name_googleplay (* twitter:app:name:googleplay *)
  | Twitter_app_id_googleplay   (* twitter:app:id:googleplay *)
  | Twitter_app_url_googleplay  (* twitter:app:url:googleplay *)

  | Unknown_meta of string * string

exception Unknown_property of string

val to_string : t -> string

val of_string : string -> t

val extract
  : ?fallback:(Nethtml.document -> unit)
  -> ?tbl:(t, string) Hashtbl.t
  -> Nethtml.document list
  -> (t, string) Hashtbl.t

val extract_from_string
  : ?fallback:(Nethtml.document -> unit)
  -> ?tbl:(t, string) Hashtbl.t
  -> string
  -> (t, string) Hashtbl.t
