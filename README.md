A long time ago, in a [Dot.com
bubble](https://en.wikipedia.org/wiki/Dot-com_bubble) far, far away...

I was still working with [Tandem
NonStop](https://en.wikipedia.org/wiki/NonStop_(server_computers))
systems. The filesystem only supported {volume, subvolume, file}. They
did not have make. They did not have... much of anything; at least not
to someone with [*nix](https://en.wikipedia.org/wiki/Unix-like)
experience looking for
[*nix](https://en.wikipedia.org/wiki/Unix-like)-ish things. I was
getting tired of configuring test environments and they most certainly
did not have [Puppet](https://en.wikipedia.org/wiki/Puppet_(software))
(not even A Thing In The World until 2005). So I wrote my
own. Kinda. Not really. No where near anything as general purpose nor even remotely the same scale ... but it allowed for a declartive statement of the desired state of the test environment and the logic to get from "here to there", but this was all only reallu useful for just to me. I was mostly looking for an excuse to play with Prolog and it seemed that Prolog would allow for a general purpose programmable, declarative specification.
So I did it via Lisp ;-) because why not make it weird?
I still did a lot of work interactively; i.e. not everything found its way
into an ASDF specification. Anyway, it served the biggest goal for me: I learnt a little bit about Prolog, got to use some Lisp, too, and, most importantly, it was fun :)
