# pinpon

`pinpon` is a silly little service that implements an
Internet-enabled doorbell in Haskell, using
[Amazon Simple Notification Service](https://aws.amazon.com/sns/) to notify
subscribers that the button has been pushed. Effectively, it's a
simple REST service which, when `POST`ed to, will send a notification
to an SNS topic. You can then build a client application which
subscribes to that topic and notifies the user when the doorbell has
been pressed. No such client application is included in the `pinpon`
package, but an iOS app may be made available at some point in the
future.

The package provides a `pinpon-gpio` executable, intended for use on
Linux systems with GPIO functionality. When the specified GPIO pin is
triggered (e.g., via a momentary switch such as
[this one](https://www.e-switch.com/product-catalog/anti-vandal/product-lines/pv3-series-illuminated-sealed-long-life-anti-vandal-switches#.WHW8_7GZNE4)),
`pinpon-gpio` will `POST` a notification to the specified `pinpon`
server.

Why not simply build the Amazon SNS functionality into the
`pinpon-gpio` executable and eliminate the `pinpon` REST service?
Chiefly because the host system running the `pinpon-gpio` executable
may be particularly vulnerable to physical attacks (after all, it is
presumably hooked up to a doorbell button that is exposed in a public
space). I did not feel comfortable storing my Amazon AWS credentials
on such a device, nor even allowing such a device to communicate
directly with the public Internet. By proxying the AWS access via a
more physically secure host running the `pinpon` server on my internal
network, I can better protect my AWS credentials and limit network
access on the GPIO device to just the `pinpon` service.

# Caveats

`pinpon` is far from ready for prime-time, production use. At this
time, it's basically a quick little hack. I do not recommend that you
use it in its current state.
