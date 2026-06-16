
# Introduction

Ocsigen Toolkit provides various user interface widgets and related utilities that assist in the rapid development of interactive Web applications.

Ocsigen Toolkit is built with [Eliom](https://ocsigen.org/eliom/).


## Installation and getting started

You can install Ocsigen Toolkit via OPAM:

```
opam install ocsigen-toolkit
```
You may want to use Ocsigen Toolkit in conjunction with [Ocsigen Start](https://ocsigen.org/ocsigen-start/), which provides an application template for quickly getting started with Ocsigen. The template provides various runnable examples of Ocsigen Toolkit widgets. See the [Ocsigen Start manual](https://ocsigen.org/ocsigen-start/latest/intro.html) for details.

See the widgets in action in [Ocsigen Start's demo application](https://ocsigen.org/ocsigen-start/demo/) (also available for Android and iOS, or in your mobile browser).


## Programming style

Most of the Ocsigen Toolkit widgets can be produced invariably on the server or on the client (with the same code). This enables a mobile-friendly programming paradigm, where most code lies in shared sections. The server instance of the code can be used to produce pages (with Ocsigen Toolkit widgets) during traditional Web interaction, while the client instance can be used to render the same pages and widgets on a mobile device without contacting the server. See the [mobile applications section](https://ocsigen.org/eliom/latest/mobile-apps.html) of the Eliom manual for details.

The widgets generally follow a reactive programming style. We use `Eliom_shared` extensively, which allows us to produce this reactive content on both sides. See [the respective manual chapter](https://ocsigen.org/eliom/latest/clientserver-react.html) for more info. `Eliom_shared` signals and events appear in the Ocsigen Toolkit APIs, and can be used as a mechanism for composing different widgets.


## CSS

Most widgets need appropriate CSS to display properly. We provide default CSS files, normally installed in

`~/.opam/${SWITCH}/share/ocsigen-toolkit/css/`

Ocsigen Start uses these files by default. If your application does not use Ocsigen Start, you will need to include the CSS manually.

Of course, you are free to modify the style to suit the desired look.


## Widgets overview

- [`Ot_buttons`](./ocsigen-toolkit.server/Ot_buttons.md): provides a dropdown menu widget
- [`Ot_calendar`](./ocsigen-toolkit.server/Ot_calendar.md): calendar widget, allowing the user to pick dates
- [`Ot_carousel`](./ocsigen-toolkit.server/Ot_carousel.md): container for blocks, only one of which is displayed at a time, with various ways to move between them (buttons, swipe, keyboard arrows)
- [`Ot_tongue`](./ocsigen-toolkit.server/Ot_tongue.md): swipable element appearing from one side of the screen
- [`Ot_color_picker`](./ocsigen-toolkit.server/Ot_color_picker.md): color picker widget
- [`Ot_drawer`](./ocsigen-toolkit.server/Ot_drawer.md): a drawer menu that typically appears on an edge of the screen. It can appear/disappear via buttons or by swiping.
- [`Ot_picture_uploader`](./ocsigen-toolkit.server/Ot_picture_uploader.md): user interface for uploading pictures
- [`Ot_popup`](./ocsigen-toolkit.server/Ot_popup.md): popup windows that can be controlled in various ways
- [`Ot_range`](./ocsigen-toolkit.server/Ot_range.md): widget for picking one among a range of values
- [`Ot_spinner`](./ocsigen-toolkit.server/Ot_spinner.md): a spinner that appears while we wait for "slow" HTML content to be generated
- [`Ot_swipe`](./ocsigen-toolkit.server/Ot_swipe.md): make element swipeable on touch screens
- [`Ot_time_picker`](./ocsigen-toolkit.server/Ot_time_picker.md): clock-like widget that allows the user to pick a time
- [`Ot_toggle`](./ocsigen-toolkit.server/Ot_toggle.md): binary toggle widget

### Non-widget utilities

- [`Ot_nodeready`](./ocsigen-toolkit.server/Ot_nodeready.md): produces an Lwt thread allowing one to wait for a node to be inserted in the DOM
- [`Ot_noderesize`](./ocsigen-toolkit.server/Ot_noderesize.md): listen to element resize events
- [`Ot_size`](./ocsigen-toolkit.server/Ot_size.md): utilities to deal with DOM element dimensions
- [`Ot_lib`](./ocsigen-toolkit.server/Ot_lib.md): functions useful for other widgets
- [`Ot_sticky`](./ocsigen-toolkit.server/Ot_sticky.md): make elements "sticky", i.e., do not let them go out of sight
- [`Ot_style`](./ocsigen-toolkit.server/Ot_style.md): an interface to `Window.getComputedStyle()`