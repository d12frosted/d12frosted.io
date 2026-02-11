We started with twenty-one volunteers and a two-week experiment. Five weeks later we had eighty-six developers across four countries, and a collection of numbers that surprised us more than expected. I'd been advocating for Claude Code for a while - I'd already used it to build several work services from scratch, validated our entire onboarding guide to the point where Claude could complete it without help. Not side projects, actual production work. I knew its value was huge. But personal conviction isn't evidence. We needed data.

# Why We Ran the Experiment

Wix engineering had a fragmented AI tooling landscape. Cursor was available and widely used. We also had an internal tool called Astra - and I want to be fair to that team. They were essentially trying to build something like Claude Code before Claude Code existed: a local web interface for AI-assisted development. Small team, limited resources, genuinely novel attempt at a hard problem. It worked, but not as well as we needed for real-world tasks.

After some internal debates about whether to try Claude Code more broadly, we decided to run a proper evaluation. I didn't want to go blindly. I wanted to measure things, understand effects and collect information. Otherwise my enthusiasm would stay just with me.

# The Design

The experiment had four components:

1.  **Limited participants in waves.** We started with twenty-one volunteers, then expanded in controlled waves - different countries, different prior tool experience - to see if effects held across contexts.

2.  **GitHub data to measure output.** Commits, PRs, lines changed. Not a perfect proxy for productivity, but measurable. Each participant compared against their own 90-day baseline, and against matched Cursor users as controls.

3.  **Feedback forms at multiple checkpoints.** Since GitHub metrics aren't the whole picture, we surveyed participants at baseline, week 1, week 2, and exit. Satisfaction, productivity perception, what worked, what didn't.

4.  **Slack channel and regular syncs.** Live feedback capture. Weekly calls with each cohort to surface issues, hear stories, understand what the numbers couldn't tell us.

# The Waves

Wave 1 started December 22nd with twenty-one volunteers. Wave 2 added twenty-seven participants. Wave 3 brought in twenty-four more, specifically recruiting heavy Cursor users. Wave 4 targeted heavy Astra users with fourteen participants.

This wasn't the original plan. We'd intended a simple two-week study. But the early results were strong enough that people started asking to join, and we said yes. The rolling design turned out to be analytically useful: each participant serves as their own baseline, which avoids calendar confounds like holidays.

# GitHub Data: What We Built

I built a dashboard called TACOS - Tracking AI Code Output Statistics. It ingested GitHub data for the entire organisation: over a million commits and PRs across 2,300+ repositories for the period of interest. Computed before-and-after metrics for each participant. Ran Difference-in-Differences analysis against matched control groups at multiple levels.

The whole pipeline ran on my laptop. I was lazy, and iterating locally was fast. I treated it as a throwaway tool from day one.

There were interesting technical challenges. Connecting commits to employees sounds trivial until you realise people forget to configure their git author properly. Personal cryptic emails, GitHub noreply addresses, mismatched identities. I used internal services and a local CLI to map commit data to employees. Resolving 89% of commit emails to actual employees took more work than the analysis itself.

Not to mention the infrastructural limitations: disk space on my laptop, API rate limits, network constraints. I couldn't afford to fully clone the entire organisation's repositories. Had to be smart about what to fetch and when.

Of course I would have loved richer data - tickets closed, cycle time, that sort of thing. But at Wix we don't constrain people with heavy tooling requirements, and we don't collect that kind of performance data. I consider this a good thing. I pity employees at companies where they have "performance metrics" tracking their every move.

So I measured what was measurable: GitHub activity.

## GitHub Results

| Metric      | Claude Code | Cursor (control) | DiD Effect |
|-------------|-------------|------------------|------------|
| PRs/day     | +92%        | +22%             | +0.61      |
| Commits/day | +119%       | +6%              | +1.11      |

These effects were consistent across all matching levels - guild, guild + country, guild + company, same manager-of-manager. Even when we restricted the control group to the top 100 heaviest Cursor users in our Server guild, the pattern held.

**Cursor barely moved the needle.** PRs essentially flat. Commits essentially flat. We'd assumed Cursor users were already seeing dramatic productivity gains, and Claude would need to beat that benchmark. Turns out the baseline assumption was wrong. It wasn't only me who noticed - a colleague independently analysed all the Cursor data and found almost no correlation between Cursor usage and output.

The funny thing: Cursor users *said* they'd become much more productive. I couldn't find proof. Meanwhile, Claude users' perception was "well, we're slightly more productive in terms of output" - while the data showed dramatic improvement. Perception and reality running in opposite directions.

**Claude's effect took time to appear.** Initially Claude users showed the same flat pattern as Cursor. But as people started to trust it more, stopped treating it like a junior developer they needed to watch constantly - you know, like hiring a team and then sitting all day watching their monitors, asking only one dev to work at a time because you can't see everything - the effect emerged. Trust unlocked productivity.

# Survey Data: What We Learned

| Metric                             | Value                             |
|------------------------------------|-----------------------------------|
| Satisfaction (baseline → exit)     | 3.77 → 4.80                       |
| NPS                                | +93 (93% promoters, 0 detractors) |
| Would keep using (week 2)          | 100%                              |
| Reported productivity gains (exit) | 100% (60% said "much more")       |
| Hours saved per week               | Median 10-20 hours                |
| Learning curve                     | 69% trivial/gentle, 0% steep      |
| Chose Claude as single AI tool     | 100%                              |

NPS (Net Promoter Score) measures how likely people are to recommend a tool, on a scale from -100 to +100. Anything above +50 is considered excellent. When asked to pick one AI coding tool, every single exit respondent chose Claude Code.

**People didn't need guidance.** My superiors assumed I was productive with Claude because I knew our infrastructure well, and others would need significant hand-holding. Wrong. People figured it out themselves, often within days. I'm glad we took the approach of giving nothing at all and letting people discover what worked.

Forty-seven percent of exit survey respondents said they'd accomplished something they wouldn't have tried before the experiment. Not "did faster" - *wouldn't have tried*.

One participant described fixing three flaky tests while sitting in a meeting. Another talked about refactoring code he'd wanted to refactor for months but never had time. A third started contributing to unfamiliar codebases: "Instead of asking other teams to fix stuff, I just go to their code, fix it myself, and submit."

AI isn't really about speed. It's about scope. It's about enabling work that previously felt too tedious, too unfamiliar, or too far outside your usual domain. We even have product people - non-developers - who started contributing code.

# Live Feedback: What Emerged

The Slack channel and weekly syncs captured things surveys couldn't.

I'm glad we decided not to give detailed guidelines. Before you can define best practices, you need to let the wild west happen. Let people find what works. Collect the collective wisdom afterward.

Within two weeks the channel was self-organising. Participants helping each other, sharing workflows, solving problems we hadn't anticipated. Best practices emerged faster than we could have documented them:

- "Start vanilla. For anything you feel that you're doing manually, try to create a skill that will make Claude do it for you."
- "Don't rush into coding. Give it some background, tickets, context."
- "Design, don't prompt."

The community built itself. We just provided the space.

## What Didn't Work

Some developers missed having a proper IDE interface. The terminal-based workflow polarised people: those who loved it really loved it, and those who didn't felt constrained. I think this is mostly about what you're used to. Giving up familiar tools is genuinely hard - even when the new thing is better, the transition cost is real. And the tooling is improving fast. IDE integrations, better UIs, more options. The market will figure this out. But the struggle for new users shouldn't be dismissed.

# The Organic Growth

The experiment escaped our control - in a good way.

I'd introduced Claude gradually. Partly as a proper experiment design, partly because finance wasn't thrilled about 150 licences for three months. But this artificial scarcity worked unexpectedly well. People with access walked around telling others how good it was. Others wanted in. I kept saying "sorry, waiting list." The closed beta created its own momentum.

Server guild had experience with Astra, and when those users tried Claude, the contrast was stark. One participant described it as "sky and earth." Another just said "one love."

The combination worked: external hype (everyone's been talking about Claude for months), internal hype (people sharing successes in channels, at meetings, in kitchens), and artificial scarcity (the closed beta that made people want in). But none of that would have mattered if the tool didn't deliver results on day one.

Hype is fun, but I understand scepticism. Some people were ready to dive in immediately; others resisted. The only thing that actually converted sceptics was the tool working. If it produces good results from the first task, you don't need to convince anyone.

# What's Next

The experiment answered the question. We're rolling out Claude Code to everyone.

But there's a new challenge emerging, and it reminds me of [the Lisp Curse](https://www.winestockwebdesign.com/Essays/Lisp_Curse.html). The original essay argues that Lisp is so powerful that problems which are technical issues in other languages become social issues in Lisp. When individuals can easily build anything themselves, coordination breaks down. Everyone rolls their own 80% solution. Documentation suffers. Collaboration becomes optional.

AI creates a similar dynamic. When individual productivity explodes, it becomes easier to work solo and locally. CI/CD pipelines - designed for a slower pace - groan under the load. Multiple teams flagged this independently during our experiment: the tool worked, but the shared infrastructure couldn't keep up.

The bottleneck shifts from "can I build this?" to "can we coordinate, review, and deploy this?" Decision-making becomes the constraint. It's hard to agree on anything in a large organisation, and now that friction is a huge bottleneck.

If we don't address this, we get the AI Curse: individuals highly productive, organisation unable to absorb the output.

So the next phase isn't just "give everyone Claude." It's improving infrastructure to be AI-friendly. Cutting the number of decision-making checkpoints - what Aviran Mordo describes as the [xEngineer](https://www.aviransplace.com/post/embracing-the-ai-revolution-introducing-the-xengineer-at-wix): fewer people involved in each process, each person owning more end-to-end. Building skills and documentation that encode our patterns. I've been running separate experiments on teaching Claude to generate our internal API patterns correctly, and building a CI system that runs scenarios against real-world tasks to catch regressions before they ship.

The dashboard keeps running. The community keeps growing. And somewhere in the backlog, there are questions we haven't answered yet: how do you validate AI-generated code at scale? How do you handle the review bottleneck when PRs contain "too much value"? What happens to expertise when the tool handles execution?

I've also been running separate experiments on teaching Claude to generate domain-specific code patterns correctly - getting it to look things up instead of guessing. If there's interest, I can write about that separately.

I don't have answers to those yet. But the experiment gave us enough confidence to keep going.

------------------------------------------------------------------------

*If you're running a similar evaluation and want to compare notes - methodology, tooling, findings - feel free to reach out.*
