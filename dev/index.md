# Ocsigen Toolkit

Ocsigen Toolkit is a set of reusable user-interface widgets for `Eliom` applications. The widgets can be produced on the server or on the client from the same code, which makes them particularly suited to mobile, client-server applications.

Ocsigen Toolkit is part of the [Ocsigen project](https://ocsigen.org).

## Installation and getting started

You can install Ocsigen Toolkit via OPAM:

```ocaml
opam install ocsigen-toolkit
```
You may want to use Ocsigen Toolkit in conjunction with `Ocsigen Start`, which provides an application template for quickly getting started with Ocsigen. The template provides various runnable examples of Ocsigen Toolkit widgets. See the `Ocsigen Start manual` for details.

See the widgets in action in [Ocsigen Start's demo application](https://ocsigen.org/ocsigen-start/demo/) (also available for Android and iOS, or in your mobile browser).

## Programming style

Most of the Ocsigen Toolkit widgets can be produced invariably on the server or on the client (with the same code). This enables a mobile-friendly programming paradigm, where most code lies in shared sections. The server instance of the code can be used to produce pages (with Ocsigen Toolkit widgets) during traditional Web interaction, while the client instance can be used to render the same pages and widgets on a mobile device without contacting the server. See the `mobile applications section` of the Eliom manual for details.

The widgets generally follow a reactive programming style. We use `Eliom.Shared` extensively, which allows us to produce this reactive content on both sides. See `the respective manual chapter` for more info. `Eliom.Shared` signals and events appear in the Ocsigen Toolkit APIs, and can be used as a mechanism for composing different widgets.

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
- [`Ot.Picture_uploader`](./ocsigen-toolkit.server/Ot-Picture_uploader.md): user interface for uploading pictures
- [`Ot.Popup`](./ocsigen-toolkit.server/Ot-Popup.md): popup windows that can be controlled in various ways
- [`Ot.Range`](./ocsigen-toolkit.server/Ot-Range.md): widget for picking one among a range of values
- [`Ot.Spinner`](./ocsigen-toolkit.server/Ot-Spinner.md): a spinner that appears while we wait for "slow" HTML content to be generated
- [`Ot.Swipe`](./ocsigen-toolkit.server/Ot-Swipe.md): make element swipeable on touch screens
- [`Ot.Time_picker`](./ocsigen-toolkit.server/Ot-Time_picker.md): clock-like widget that allows the user to pick a time
- [`Ot.Toggle`](./ocsigen-toolkit.server/Ot-Toggle.md): binary toggle widget

### Non-widget utilities

- [`Ot.Nodeready`](./ocsigen-toolkit.server/Ot-Nodeready.md): produces an Lwt thread allowing one to wait for a node to be inserted in the DOM
- [`Ot.Noderesize`](./ocsigen-toolkit.server/Ot-Noderesize.md): listen to element resize events
- [`Ot.Size`](./ocsigen-toolkit.server/Ot-Size.md): utilities to deal with DOM element dimensions
- [`Ot.Lib`](./ocsigen-toolkit.server/Ot-Lib.md): functions useful for other widgets
- [`Ot.Sticky`](./ocsigen-toolkit.server/Ot-Sticky.md): make elements "sticky", i.e., do not let them go out of sight
- [`Ot.Style`](./ocsigen-toolkit.server/Ot-Style.md): an interface to `Window.getComputedStyle()`
