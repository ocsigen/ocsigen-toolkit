(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2016, Julien Sagot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

open Nethtml

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

let of_string = function
  | "og:title"                    -> Og_title
  | "og:site_name"                -> Og_site_name
  | "og:type"                     -> Og_type
  | "og:description"              -> Og_description
  | "og:locale"                   -> Og_locale
  | "og:locale:alternate"         -> Og_locale_alternate
  | "og:video"                    -> Og_video
  | "og:video:url"                -> Og_video_url
  | "og:video:secure_url"         -> Og_video_secure_url
  | "og:video:type"               -> Og_video_type
  | "og:video:with"               -> Og_video_with
  | "og:video:height"             -> Og_video_height
  | "og:image"                    -> Og_image
  | "og:url"                      -> Og_url
  | "og:image:secure_url"         -> Og_image_secure_url
  | "og:image:type"               -> Og_image_type
  | "og:image:with"               -> Og_image_with
  | "og:image:height"             -> Og_image_height
  | "fb:admins"                   -> Fb_admins
  | "twitter:card"                -> Twitter_card
  | "twitter:site"                -> Twitter_site
  | "twitter:site:id"             -> Twitter_site_id
  | "twitter:creator"             -> Twitter_creator
  | "twitter:creator:id"          -> Twitter_creator_id
  | "twitter:url"                 -> Twitter_url
  | "twitter:title"               -> Twitter_title
  | "twitter:description"         -> Twitter_description
  | "twitter:image"               -> Twitter_image
  | "twitter:image:alt"           -> Twitter_image_alt
  | "twitter:player"              -> Twitter_player
  | "twitter:player:width"        -> Twitter_player_width
  | "twitter:player:height"       -> Twitter_player_height
  | "twitter:player:stream"       -> Twitter_player_stream
  | "twitter:app:name:iphone"     -> Twitter_app_name_iphone
  | "twitter:app:id:iphone"       -> Twitter_app_id_iphone
  | "twitter:app:url:iphone"      -> Twitter_app_url_iphone
  | "twitter:app:name:ipad"       -> Twitter_app_name_ipad
  | "twitter:app:id:ipad"         -> Twitter_app_id_ipad
  | "twitter:app:url:ipad"        -> Twitter_app_url_ipad
  | "twitter:app:name:googleplay" -> Twitter_app_name_googleplay
  | "twitter:app:id:googleplay"   -> Twitter_app_id_googleplay
  | "twitter:app:url:googleplay"  -> Twitter_app_url_googleplay
  | "description"                 -> Description
  | u                             -> raise (Unknown_property u)

let to_string = function
| Title -> "title"
| Description -> "description"
| Og_title -> "og:title"
| Og_site_name -> "og:site:name"
| Og_type -> "og:type"
| Og_description -> "og:description"
| Og_locale -> "og:locale"
| Og_locale_alternate -> "og:locale:alternate"
| Og_video -> "og:video"
| Og_video_url -> "og:video:url"
| Og_video_secure_url -> "og:video:secure:url"
| Og_video_type -> "og:video:type"
| Og_video_with -> "og:video:with"
| Og_video_height -> "og:video:height"
| Og_image -> "og:image"
| Og_url -> "og:url"
| Og_image_secure_url -> "og:image:secure:url"
| Og_image_type -> "og:image:type"
| Og_image_with -> "og:image:with"
| Og_image_height -> "og:image:height"
| Fb_admins -> "fb:admins"
| Twitter_card -> "twitter:card"
| Twitter_site -> "twitter:site"
| Twitter_site_id -> "twitter:site:id"
| Twitter_creator -> "twitter:creator"
| Twitter_creator_id -> "twitter:creator:id"
| Twitter_url -> "twitter:url"
| Twitter_title -> "twitter:title"
| Twitter_description -> "twitter:description"
| Twitter_image -> "twitter:image"
| Twitter_image_alt -> "twitter:image:alt"
| Twitter_player -> "twitter:player"
| Twitter_player_width -> "twitter:player:width"
| Twitter_player_height -> "twitter:player:height"
| Twitter_player_stream -> "twitter:player:stream"
| Twitter_app_name_iphone -> "twitter:app:name:iphone"
| Twitter_app_id_iphone -> "twitter:app:id:iphone"
| Twitter_app_url_iphone -> "twitter:app:url:iphone"
| Twitter_app_name_ipad -> "twitter:app:name:ipad"
| Twitter_app_id_ipad -> "twitter:app:id:ipad"
| Twitter_app_url_ipad -> "twitter:app:url:ipad"
| Twitter_app_name_googleplay -> "twitter:app:name:googleplay"
| Twitter_app_id_googleplay -> "twitter:app:id:googleplay"
| Twitter_app_url_googleplay -> "twitter:app:url:googleplay"
| Unknown_meta (a, b) -> "Unknown (\"" ^ a ^ "\"=\"" ^ b ^ "\")"

let extract ?(fallback = fun _ -> ()) ?(tbl = Hashtbl.create 44) x =
  let rec extract x =
    match x with
    | Element ("html", _, c)
    | Element ("head", _, c) -> List.iter extract c
    | Element ("title", _, [ Data d ]) -> Hashtbl.add tbl Title d
    | Element ("meta", attr, _) ->
       begin
         try
           let property =
             try of_string (List.assoc "property" attr)
             with
             | Unknown_property u -> Unknown_meta ("property", u)
             | Not_found ->
                try of_string (List.assoc "name" attr)
                with Unknown_property u -> Unknown_meta ("name", u) in
           Hashtbl.add tbl property (List.assoc "content" attr)
         with Not_found -> fallback x end
    | _ -> fallback x in
  let () = List.iter extract x in
  tbl

let extract_from_string ?fallback ?tbl s =
  let ch = new Netchannels.input_string s in
  let docs = Nethtml.parse ?return_pis:(Some false) ch in
  ch # close_in () ;
  extract ?fallback ?tbl docs
