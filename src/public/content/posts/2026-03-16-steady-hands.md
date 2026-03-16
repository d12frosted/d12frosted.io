*On AI, speed, and the discipline we forgot to talk about.*

—

There is an image that keeps returning to me. An astronomical telescope, handheld.

The optics are extraordinary. You can see craters on moons, the rings of distant planets, things no unaided eye could reach. But the more powerful the magnification, the more any tremor in your grip displaces the view - not by millimetres, but by thousands of kilometres. A slight shift of the wrist and you're looking at empty space where a moment ago there was a star.

The tool didn't make seeing harder. It made *steadiness* harder. And if you don't know that going in, you'll think the sky is shaking when it's actually your hands.

I keep thinking about this image because I got something wrong a few months ago. I wrote that AI might be eroding expertise - that the gap between capability and comprehension was widening, quietly, and that the people most tempted to skip the foundations were the ones who needed them most. I called it "[knowing less, producing more](/posts/2025-12-08-knowing-less-producing-more)." I meant it as an observation, not a verdict. But observations have a way of settling into conclusions if you don't revisit them.

I no longer think it's erosion. I think it's something louder and more disorienting - a transformation that *feels* like erosion because the speed distorts our sense of what's happening. The principles of good engineering haven't changed. The cost of ignoring them has. The telescope got stronger. The sky stayed where it was.

—

There is an argument I keep encountering - that AI has made engineering easier because "scaffolding is free now." I find this puzzling. I work at a company where spinning up a new service has been trivial for six years. A newcomer can deploy a basic CRUD service in their first weeks without talking to anyone. Once you've absorbed the vocabulary and gotten familiar with the infrastructure, the next one takes an hour - and most of that is CI. The skeleton is deterministic: same structure, same pipeline, same observability hooks, every time. If this still requires mental gymnastics somewhere in 2026, the problem isn't a lack of AI. It's a lack of infrastructure.

But scaffolding was always the least interesting part. It's the visible tip of something much larger and quieter: platform engineering. The real work of a platform is encoding context - vast, accumulated, hard-won context about how things should behave - into shared infrastructure so that individual engineers don't have to hold it themselves. You need to call another service? You don't care about the details of service discovery, retries, circuit breaking, tracing. You care about *who* to call and *what* their API looks like. Give the platform the coordinates and it connects everything. Listening to events, persistence, change data capture - these are solved problems with enormous hidden complexity, and the whole point is that you shouldn't be solving them again. The platform absorbs the context. You think about business value.

This is where AI genuinely changes things - not in the infrastructure, which should be deterministic and invisible regardless, but in the business logic. The part that's different each time, that requires understanding the domain, that benefits from someone who can articulate what they want and recognise whether they got it. A frontend engineer can now implement non-trivial server-side logic without deeply knowing the runtime. A backend engineer can build a reasonable UI without years of CSS archaeology. The boundaries between profiles are dissolving - not because the boring parts got easier, but because the cognitive cost of real work outside your specialisation has dropped.

And platform engineering doesn't become less relevant in this world. It becomes *more* relevant - perhaps shifting to care less about the volume of generated code and more about compilation speed, correctness guarantees, making sure the platform remains the path of least resistance. Because when code is cheap, the temptation to bypass the platform and roll your own is enormous. And that temptation is where the telescope starts to matter.

—

Code is cheap now. It costs almost nothing to write something. And there is a pattern I keep seeing: new users arrive, they are blown away by what AI can produce, and they start creating. New services, new features, new tools. The output is impressive. The velocity is intoxicating.

And then the problems begin.

Not because the code is bad - often it isn't. But because cheap code doesn't mean cheap coordination. Consider something as mundane as GDPR compliance. In isolation, implementing it is trivial. But in a large company, every service handling user data must behave in exactly the same way. The same deletion flows, the same consent mechanisms, the same audit trails. When code was expensive, this happened slowly and with coordination - a shared library, a platform decision, a guild discussion. When code is cheap, twelve engineers solve it twelve different ways in an afternoon, each implementation correct in isolation and collectively a nightmare.

The fact that you *can* do something doesn't mean you *should*. This is the Lisp Curse applied to the age of AI: when the tool is powerful enough that any individual can build anything, the incentive to coordinate dissolves. Everyone retreats to their own corner with their own solution that handles eighty percent of the problem. The remaining twenty percent, which requires talking to other humans and agreeing on shared abstractions, never gets done.

Steadiness, it turns out, is not only about aim - knowing where to point the telescope. It's also about restraint - resisting the urge to swing it toward every interesting thing in the sky. The GDPR example isn't a failure of direction. It's a failure of discipline. Twelve engineers all pointed at the right star. They just each built their own telescope to look at it.

DRY still holds. Platform engineering still matters. Compilers that prevent mistakes still matter. Linters, tests, shared libraries, deterministic scaffolding - all of it still holds. None of it became less important because code got cheap. If anything, it became more important, because violations now propagate at machine speed across a hundred services before anyone notices.

Code is cheap. Context is not. Our ability to comprehend didn't change. And faster pace means *more* tax on our brains, not less. The telescope is more powerful than ever. The hands holding it need to be steadier than ever.

—

There is an anxious discourse happening right now about what AI means for engineering managers - how the role is "dissolving," how sprints are over by Wednesday, how process ownership has lost its meaning. I find most of it hollow. Not wrong, exactly - more like asking the wrong question with great confidence. If you defined your value through designing sprints and running retros, then yes, AI compresses the part of the cycle you were wrapped around. But this was never the interesting part of the role. Everyone is becoming a kind of project manager now - articulating outcomes, defining context, reviewing results. This isn't a crisis. It's Tuesday. The real question, the one the management discourse keeps avoiding, is not "how do managers survive?" but "how does anyone maintain the expertise to know what good looks like?"

—

I am effective with AI because I have years of experience behind me. I can spot patterns - good or bad - before they land in production. I have intuitions about architecture that I can't always articulate but that consistently prove right. When AI proposes a solution, I can feel whether it's structurally sound or merely plausible. This isn't magic. It's the residue of thousands of hours of debugging, designing, getting things wrong, and slowly learning to get them right.

This is what I was worried about in my earlier essays: that AI removes the friction through which this expertise forms. And I still think the concern is real. But I was framing it as loss. Now I think the framing should be different.

It's not that expertise is eroding. It's that expertise is being *redefined* - loudly, confusingly, in real time - and the noise of the transformation sounds like erosion if you're not listening carefully.

What's actually happening is that some skills are migrating and others are becoming more important than they ever were. The mechanical skills - writing boilerplate, remembering API signatures, configuring build pipelines - these are leaving. Fine. They were never the core of the craft. What remains, and what intensifies, is the skill of aim. Knowing where to point the telescope. Knowing what question to ask. Knowing what context matters and what can be safely ignored. Knowing when to delegate and when to struggle.

—

Working with AI is not unlike working with people. Each new session is a newcomer who knows nothing about your context. They learn fast - faster than any human would - but they arrive blank. And unlike a human, they won't remember tomorrow. Your job is to onboard them: articulate the outcome you want clearly enough that they can move toward it, and understand your domain deeply enough to know what context is critical to share. Which constraints are load-bearing. Which decisions have history behind them that the code alone doesn't reveal.

This is the same skill you need when onboarding a junior engineer, when explaining a technical decision to a product manager, when writing a design document that will outlive the meeting where you presented it. And it depends, entirely, on the expertise you bring. Without understanding the craft, it's hard to know what context matters. You might explain the wrong things in great detail while leaving the critical assumptions implicit. The AI will do exactly what you described - competently, thoroughly, and in the wrong direction.

And here is where the thread connects back to platform engineering. A good platform constrains an engineer's choices - not to limit capability, but to prevent drift. It makes the right path the easy path, so that you don't need to hold the full context of service discovery or event handling or data retention to make a correct decision. Working with AI well requires the same principle: you constrain the decision space. You provide the CLAUDE.md, the architecture document, the shared conventions. You reduce the surface area where the AI can wander, so that its energy goes toward the part that actually requires creativity. The platform prevents drift at the infrastructure level. The human prevents drift at the intent level. Both are acts of steadiness.

AI didn't bring much new to the table in terms of mental models. There are new challenges, a new pace. But fundamentally it's the same discipline, multiplied by speed and scale.

—

And this is where I keep landing: the discipline was always the point.

When I wrote about [the struggle you choose](/posts/2026-02-02-the-struggle-you-choose), I was circling this idea - that the people who use AI most effectively aren't using it to skip thinking, they're using it to think about different things. The mechanical parts get handled. The architectural parts get more attention. The overall pace stays human, because humans are the bottleneck, and that's fine.

But now I'd add something. It's not just that the pace stays human. It's that the *stakes* of human judgment increase. If you can reach ten times further with AI, then the direction you're pointing in matters ten times more. A small error in aim, magnified by the telescope, puts you thousands of kilometres off target. And the restraint not to aim everywhere at once matters ten times more too.

This is why I think the conversation should shift. Not "are managers obsolete?" Not "are profiles irrelevant?" Not "is expertise eroding?" These are all surface questions that mistake speed for substance.

The deeper question is about discipline. Mental discipline, specifically. The discipline to not build something just because you can. The discipline to coordinate when it's easier to go alone. The discipline to maintain shared abstractions when every individual could roll their own in an afternoon. The discipline to sit with a problem long enough to understand it before reaching for the tool that will solve it instantly.

We were always supposed to have this discipline. We just got away with not having it, because the speed of production was slow enough to absorb our imprecision. The telescope was weak enough that shaky hands didn't matter.

Now it matters.

—

I should be honest about what I don't know.

Every conversation about AI and its impact eventually bumps against the same wall: we are historically terrible at predicting what technological revolutions actually do to labour and society. The confident predictions - about which roles disappear, which skills become obsolete, which industries transform - are almost always wrong in their specifics, even when they're right in their general direction. We sense the wind but misread where it's blowing.

The revolution, when it arrives, tends to be a freedom *from* - from manual labour, from repetitive tasks, from the constraints that shaped the previous era. But it's rarely a freedom *to* anything specific. The "to" gets written afterward, by the people living through it, and it almost never matches what the forecasters predicted.

So I'm suspicious of anyone who claims to understand the full impact of what's happening. I'm suspicious of the pessimists who see only erosion, and of the optimists who see only liberation, and of the management consultants who see only an opportunity to sell frameworks for "the agentic age." I was partly that pessimist myself, a few months ago. I'm trying to hold the uncertainty more honestly now.

What I can observe is this: the patterns of good engineering haven't changed. The cost of ignoring them has increased. The skills that matter most are the ones that were always hardest to teach - judgment, taste, the ability to hold complexity, the willingness to coordinate with other humans when it would be easier not to. AI amplifies whatever you bring to it: discipline or carelessness, vision or drift, aim or tremor.

The telescope is extraordinary. What it demands of us is not new. It's just no longer optional.

—

*The hands need to be steady. The rest follows from that.*
