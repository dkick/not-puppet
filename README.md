A long time ago, in a [Dot.com
bubble](https://en.wikipedia.org/wiki/Dot-com_bubble) far, far away...

I was still working with [Tandem
NonStop](https://en.wikipedia.org/wiki/NonStop_(server_computers))
systems. Tandem did have their own version of a [*nix](https://en.wikipedia.org/wiki/Unix-like), IIRC, but we weren't using it. The filesystem only supported {volume, subvolume, file}. They
did not have make. They did not have... much of anything; at least not
to someone with [*nix](https://en.wikipedia.org/wiki/Unix-like)
experience looking for
[*nix](https://en.wikipedia.org/wiki/Unix-like)-ish things. We had a complicated build process, with a cross compile step, which then requried a second pass on the Tandem, and then more steps after that for install, etc. I was
getting tired of configuring test environments and they most certainly
did not have something like a [Puppet](https://en.wikipedia.org/wiki/Puppet_(software))
(not even A Thing In The World until 2005). I wanted something to kick off the cross compile, and then exec the remote steps afterwards, but in a make-like, declarative fashion. So I wrote my
own. Kinda. Not really. No where near anything as general purpose nor even remotely the same scale ... but it allowed for a declartive statement of the desired state of the test environment and the logic to get from "here to there", but this was all only really useful for just to me. I was mostly looking for an excuse to play with Prolog; it seemed that Prolog would allow for a general purpose programmable, declarative specification.
So I did it via Lisp, ;-) because why not make it weird? Allegro CL provided [Allegro Prolog](https://franz.com/support/documentation/current/doc/prolog.html), which was a way for me to get to use "two birds with one stone," as it were.
It was always a bit half baked: I still did a lot of work interactively in a [SLIME REPL](https://slime.common-lisp.dev); not everything found its way
into an ASDF specification, so I would load a few things "by hand"; etc. Anyway, I did automate a lot of the build and setup but it served the biggest goal for me: I learnt a little bit about Prolog, got to use Common Lisp, too, and, most importantly, it was fun :)
