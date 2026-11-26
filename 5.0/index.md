# Ocsigen Toolkit

Ocsigen Toolkit is a set of reusable user-interface widgets for [Eliom](./../eliom/index.md) applications. The widgets can be produced on the server or on the client from the same code, which makes them particularly suited to mobile, client-server applications.

Ocsigen Toolkit is part of the [Ocsigen project](https://ocsigen.org).

## Installation and getting started

You can install Ocsigen Toolkit via OPAM:

```ocaml
opam install ocsigen-toolkit
```
Add `ocsigen-toolkit.server` and `ocsigen-toolkit.client` to the libraries of the server and client parts of your application.

You may want to use Ocsigen Toolkit in conjunction with `Ocsigen Start`, which provides an application template for quickly getting started with Ocsigen. The template provides various runnable examples of Ocsigen Toolkit widgets. See the `Ocsigen Start manual` for details.

See the widgets in action in [Ocsigen Start's demo application](https://ocsigen.org/ocsigen-start/demo/) (also available for Android and iOS, or in your mobile browser).

## Migrating from version 4

Since version 5, the modules of Ocsigen Toolkit are submodules of [`Ot`](./ocsigen-toolkit.server/Ot.md): write `Ot.Popup`, `Ot.Carousel` or `Ot.Form` instead of `Ot_popup`, `Ot_carousel` or `Ot_form`. Ocsigen Toolkit 5 also requires Eliom 13, which renames its own modules the same way (`Eliom.Content.Html` instead of `Eliom_content.Html`, for instance).

To migrate progressively, install the `ocsigen-toolkit-compat` package and add the libraries `ocsigen-toolkit-compat.server` and `ocsigen-toolkit-compat.client` to the server and client parts of your application, respectively. They define the old `Ot_xxx` modules, each of which includes the new module, so that existing code keeps compiling while you rename the references. Eliom provides the same kind of help for its own modules with the `eliom-compat` package (libraries `eliom-compat.server` and `eliom-compat.client`).

## Programming style

Most of the Ocsigen Toolkit widgets can be produced invariably on the server or on the client (with the same code). This enables a mobile-friendly programming paradigm, where most code lies in shared sections. The server instance of the code can be used to produce pages (with Ocsigen Toolkit widgets) during traditional Web interaction, while the client instance can be used to render the same pages and widgets on a mobile device without contacting the server. See the [mobile applications section](./../eliom/workflow/mobile.md) of the Eliom manual for details.

The widgets generally follow a reactive programming style. We use [`Eliom.Shared`](./../eliom/eliom.server/Eliom-Shared.md) extensively, which allows us to produce this reactive content on both sides. See [the respective manual chapter](./../eliom/clientserver/react.md) for more info. [`Eliom.Shared`](./../eliom/eliom.server/Eliom-Shared.md) signals and events appear in the Ocsigen Toolkit APIs, and can be used as a mechanism for composing different widgets.

## CSS

Most widgets need appropriate CSS to display properly. We provide default CSS files, normally installed in

`~/.opam/${SWITCH}/share/ocsigen-toolkit/css/`

Ocsigen Start uses these files by default. If your application does not use Ocsigen Start, you will need to include the CSS manually.

Of course, you are free to modify the style to suit the desired look.

## Widgets overview

- [`Ot.Buttons`](./ocsigen-toolkit.server/Ot-Buttons.md): provides a dropdown menu widget
- [`Ot.Calendar`](./ocsigen-toolkit.server/Ot-Calendar.md): calendar widget, allowing the user to pick dates
- [`Ot.Carousel`](./ocsigen-toolkit.server/Ot-Carousel.md): container for blocks, only one of which is displayed at a time, with various ways to move between them (buttons, swipe, keyboard arrows)
- [`Ot.Tongue`](./ocsigen-toolkit.server/Ot-Tongue.md): swipable element appearing from one side of the screen
- [`Ot.Color_picker`](./ocsigen-toolkit.server/Ot-Color_picker.md): color picker widget
- [`Ot.Drawer`](./ocsigen-toolkit.server/Ot-Drawer.md): a drawer menu that typically appears on an edge of the screen. It can appear/disappear via buttons or by swiping.
- [`Ot.Form`](./ocsigen-toolkit.server/Ot-Form.md): reactive form elements (inputs, checkboxes, radio buttons, selects, date and time inputs, password fields) and helpers for validation and keyboard navigation
- [`Ot.Icons`](./ocsigen-toolkit.server/Ot-Icons.md): icon elements styled by the toolkit CSS
- [`Ot.Page_transition`](./ocsigen-toolkit.client/Ot-Page_transition.md) (client only): animated transitions between pages, using screenshots
- [`Ot.Picture_uploader`](./ocsigen-toolkit.server/Ot-Picture_uploader.md): user interface for uploading pictures
- [`Ot.Popup`](./ocsigen-toolkit.server/Ot-Popup.md): popup windows that can be controlled in various ways
- [`Ot.Pulltorefresh`](./ocsigen-toolkit.server/Ot-Pulltorefresh.md): refresh an element by pulling it down
- [`Ot.Range`](./ocsigen-toolkit.server/Ot-Range.md): widget for picking one among a range of values
- [`Ot.Spinner`](./ocsigen-toolkit.server/Ot-Spinner.md): a spinner that appears while we wait for "slow" HTML content to be generated
- [`Ot.Swipe`](./ocsigen-toolkit.server/Ot-Swipe.md): make element swipeable on touch screens
- [`Ot.Time_picker`](./ocsigen-toolkit.server/Ot-Time_picker.md): clock-like widget that allows the user to pick a time
- [`Ot.Tip`](./ocsigen-toolkit.client/Ot-Tip.md) (client only): a box displayed over the page content, closed by a click outside it
- [`Ot.Toggle`](./ocsigen-toolkit.server/Ot-Toggle.md): binary toggle widget

### Non-widget utilities

- [`Ot.Nodeready`](./ocsigen-toolkit.client/Ot-Nodeready.md) (client only): produces an Lwt thread allowing one to wait for a node to be inserted in the DOM
- [`Ot.Noderesize`](./ocsigen-toolkit.client/Ot-Noderesize.md) (client only): listen to element resize events
- [`Ot.Size`](./ocsigen-toolkit.client/Ot-Size.md) (client only): utilities to deal with DOM element dimensions
- [`Ot.Lib`](./ocsigen-toolkit.server/Ot-Lib.md): functions useful for other widgets
- [`Ot.Sticky`](./ocsigen-toolkit.client/Ot-Sticky.md) (client only): make elements "sticky", i.e., do not let them go out of sight
- [`Ot.Style`](./ocsigen-toolkit.client/Ot-Style.md) (client only): an interface to `Window.getComputedStyle()`
