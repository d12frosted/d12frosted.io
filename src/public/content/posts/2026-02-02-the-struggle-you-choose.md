*On AI, speed, and the skill that doesn't have a name.*

—

There is a formula we learn in school: distance equals velocity times time. S = Vt. If you want to cover more ground, you go faster. This is the assumption most people bring to AI. It makes you faster. You produce more. You win.

A [recent study](https://arxiv.org/abs/2601.20245) from Anthropic tested this assumption in a specific context: software developers learning a new library. Half had AI assistance, half didn't. The AI group scored 17% lower on comprehension tests afterward. They understood less of what they'd built. The surprise was the other finding: they weren't significantly faster, either. The time saved by not writing code was eaten by composing queries, reading outputs, iterating with the assistant. Only those who fully delegated - pasting AI code without questions - finished faster. And they learned almost nothing.

The study is worth reading in full. But what stayed with me wasn't the headline finding. It was the taxonomy of behaviour that emerged. The researchers identified six distinct patterns of AI use, and three of them preserved learning while three destroyed it.

—

The patterns that failed looked like what most people imagine when they think "using AI to code." Full delegation: ask AI to write the solution, paste it, move on. Progressive reliance: start engaged, get tired, hand over the wheel. Iterative debugging: paste errors into the chat repeatedly until something works, never understanding why.

The patterns that worked looked different. Conceptual inquiry: ask AI only about ideas, resolve implementation problems yourself. Hybrid explanation: request code, but demand explanations alongside it. Generation-then-comprehension: let AI write the code, then interrogate it until you understand what it did.

The divide wasn't between "using AI" and "not using AI." It was between cognitive engagement and cognitive offloading. The same tool, the same interface, producing opposite outcomes depending on the relationship.

What struck me most: the high-engagement users weren't significantly slower than the delegators. They just spent their time differently. Less typing, more thinking. The velocity was similar. The **learning** diverged completely.

—

I've been circling this idea for a while now - that the framing of "AI makes you faster" misunderstands what's actually happening. Speed implies a fixed destination reached sooner. But the destination isn't fixed. AI changes what's reachable at all.

Last year I built a small library I'd been designing in my head for months. The architecture was clear to me. The implementation would have taken weeks of evenings I didn't have. With AI handling the mechanical parts - the parts I could verify but didn't need to invent - it took days. Was I faster? In some trivial sense, yes. But the more accurate description is that I built something I otherwise wouldn't have built. The counterfactual wasn't "the same library, slower." It was "no library."

This matters because the speed framing makes AI a tool for doing the same things more efficiently. Which invites the obvious question: if AI can do it faster, why do you need to do it at all? The logical endpoint is full delegation. You become a manager of outputs you don't understand.

But if the framing is **reach** - what you can now attempt that was previously beyond your constraints - then the relationship changes. AI doesn't replace your cognition. It extends it. You remain the bottleneck, but the bottleneck can now touch things it couldn't before.

—

I want to stay with that word: bottleneck. It sounds like a problem to solve. In productivity culture, bottlenecks are eliminated. You find the constraint and remove it.

But you can't remove yourself (as funny as it sounds). Your comprehension rate is fixed. Your attention is finite. You need rest. You can only hold so much complexity in your head at once before it starts leaking. These aren't bugs. They're the boundary conditions of being a person.

AI doesn't change these constraints. It changes what you can do **within** them. You still think at human speed. You still need to understand what you're building. But the time and energy that used to go into mechanical execution can now go into holding the vision, shaping the architecture, asking the next question.

This is why the high-engagement patterns in the study worked. Those users didn't try to become faster. They stayed at human pace but redirected their effort toward comprehension. They used AI to handle what they already understood while they focused on what they didn't yet.

The low-engagement patterns tried to skip the understanding entirely. And for a single task, measured only by completion, it looked like it worked. They finished. The code ran. But they emerged unchanged. The next task would find them exactly where they started.

—

The researchers noted something I keep thinking about: the biggest gap between groups wasn't in conceptual questions or code reading. It was in debugging. The AI users were dramatically worse at finding errors.

This makes sense. If AI writes your code and it works, you never see the errors that teach you how the system behaves. You never encounter the runtime exception that forces you to understand what "await" actually means. You never chase a bug through three stack frames and emerge knowing, finally, why the order of operations matters.

Errors are uninvited feedback. They arrive whether you want them or not. They don't care about your feelings or your deadline. They simply expose the gap between what you thought and what is true.

AI is optimised to minimise this friction. It wants to help, which means it wants to remove obstacles, which means it wants to give you working code without the errors that would have taught you why it works. The assistance is genuine. The cost is hidden.

This maps onto something I've noticed in how people talk about AI and relationships. The fear isn't that AI will replace human connection - not exactly. It's that AI offers connection without friction, feedback without challenge, companionship without the discomfort that forces growth. You can have a friend who never pushes back, never misunderstands you in ways that make you clarify what you actually mean, never gets tired of your patterns. And something is lost in that frictionlessness, even as something is gained.

The parallel to skill formation is almost exact. You can have a collaborator who never lets you struggle, never makes you confront your own confusion, never leaves you alone with an error you have to solve yourself. And you will produce more, perhaps. But you will not become more.

—

Here is where I should offer a solution, or at least a framework. But I'm not sure I have one that doesn't collapse into "just be disciplined." The high-engagement patterns in the study weren't taught. They emerged from some combination of disposition, experience, and - I suspect - a felt sense that understanding matters even when shipping is the goal.

What I can say is that the skill of using AI well is itself a skill. It doesn't have a name yet. It's not prompt engineering, exactly - that's too narrow. It's something like: knowing when to delegate and when to struggle. Knowing when the answer matters less than the process of reaching it. Knowing what you need to understand deeply versus what you can safely treat as a black box.

I built that intuition by building things with AI. Not by reading about it. The projects were almost beside the point. What I was learning, without realising it at first, was a new mode of collaboration. Architect and critic. Shaping problems, then letting AI propose solutions, then interrogating those solutions until I understood them or rejected them. The library I built was the artifact. The intuition was the actual output.

This is hard to talk about because it sounds like a humble-brag about productivity. It's not. It's an attempt to name something that the discourse around AI keeps missing: the people who use AI most effectively aren't using it to skip thinking. They're using it to think about different things. The mechanical parts get handled. The architectural parts get more attention. The overall pace stays human, because humans are the bottleneck, and that's fine.

—

We've always had people who coast and people who push. Some percentage of any population will take the easiest path available; some will seek challenge regardless of whether it's required. AI didn't create this divide. It just lowered the floor and raised the ceiling at the same time.

The floor is now very low. You can produce working code, passable prose, plausible analysis with almost no understanding of what you're producing. The critics who decry "AI slop" are responding to this, though they often miss that the problem isn't AI - it's the absence of a shaping mind. Full delegation produces slop because no one was thinking. The output has no structure because no one held a structure in their head.

But the ceiling is higher too. If you were already the kind of person who thinks carefully, who holds complexity, who wants to understand before you ship - you can now reach things that were previously out of range. Not because you go faster, but because the ratio of thinking to typing shifts in your favour.

Is that a problem? For whom?

I keep landing on the same question the researchers asked: how do we grow strong practitioners if the path to competence now has an easy bypass? The people who benefit most from AI are those who already have the foundations. The people who need foundations most are the ones most tempted to skip them.

I don't have an answer. But I notice that the question assumes skill formation happens through struggle - through errors, friction, uninvited feedback. If that's true, then preserving space for struggle becomes important even when tools exist to eliminate it. Not as a moral stance, but as a practical matter of developing the capacity to supervise what you can't fully understand.

Maybe that's the skill that needs teaching. Not how to use AI, but when to stop using it. When to close the chat and sit with the error. When to reach for understanding instead of reaching for help.

The tool will keep getting better. The question is whether we will.

—

*The study referenced throughout is Shen & Tamkin (2026), "[How AI Impacts Skill Formation](https://arxiv.org/abs/2601.20245)."*
