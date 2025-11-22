It's no secret that I enjoy using Emacs. People often ask me why I chose Emacs over Vi(m), and the primary reason is modal editing - I simply don't get it. I understand that some people can fully utilise this feature, but I've never been able to make it click for me.

Recently, I was reading an article on Irreal about Vim not being as hard to learn as Emacs, which linked to another interesting [piece about Vim's composability](https://medium.com/@mkozlows/why-atom-cant-replace-vim-433852f4b4d1#.rqcouk4l1) by Mike Kozlowski. I completely agree that composability is a brilliant feature that unfortunately hasn't been widely adopted. It's powerful, learnable, and consistent - just as Kozlowski states. I won't repeat his article; instead, I encourage you to read it yourself.

What I want to do here is defend Emacs (and some other editors). I think it was a very smart decision to stake everything on extensibility. Emacs is an excellent example of this approach. Just look at `evil` - whenever I think about it, I'm amazed. I don't want to use it myself, but it demonstrates that extensibility matters and that its limits are remarkably loose.

Let's return to composability in Emacs. One might argue that Emacs doesn't have built-in composability, but let's be honest - it's still [easy to implement](https://github.com/paldepind/composable.el) Vim-like composability in Emacs. It's just a matter of effort.

To follow Kozlowski's lead, I'll admit that Emacs isn't perfect. Some people dislike Emacs Lisp, and the out-of-the-box experience is far from ideal. To perfect Emacs as an editor, you need to invest considerable time in writing configurations and using dozens of third-party packages. However, this gives you freedom. And if you don't like writing configs, just look at these excellent projects: [Purcell's emacs.d](https://github.com/purcell/emacs.d), [Prelude](https://github.com/bbatsov/prelude), and of course [Spacemacs](https://github.com/syl20bnr/spacemacs). You have plenty of options, and composability is one of them.

The beauty of Emacs is that extensibility allows the community to implement whatever features they value - including composability. Whilst Vim bakes composability into its core design, Emacs provides the tools to build it yourself (or use someone else's implementation). Neither approach is inherently better; they simply reflect different philosophies about how editors should be designed.

<!--more-->
