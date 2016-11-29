---
origdate: '2015-08-25'
published: '2015-08-25'
title: How WiFi Actually Works
---

Note no tags
============

25-15:31 &lt; treed&gt; In the ideal situation for wifi, you have one
station per AP, on non-overlapping channels

25-15:31 &lt; treed&gt; which in the best possible 2.4Ghz case gets you
3 stations

25-15:31 &lt; treed&gt; but the closer you can get to that ideal, the
better

25-15:31 &lt; rlpowell&gt; "station"?

25-15:31 &lt; treed&gt; more APs and wire the fuck out of everything
that can be wired

25-15:32 &lt; treed&gt; generic term for "thing speaking wifi"

25-15:32 &lt; rlpowell&gt; I'm sorry, are you telling me that the ideal
ration of wifi consumer to WAP is 1:1 ?

25-15:32 &lt; rlpowell&gt; Because if not, I'm lost.

25-15:32 &lt; treed&gt; Absolutely.

25-15:32 &lt; Tene&gt; rlpowell: Yes; that will get you optimal
performance

25-15:32 &lt; treed&gt; Having more than 1:1 means it has to share the
air.

25-15:32 &lt; treed&gt; And sharing means collisions.

25-15:32 &lt; rlpowell&gt; OK, bear in mind that this is all magic to
me.

25-15:33 &lt; rlpowell&gt; It what?

25-15:33 &lt; treed&gt; Remember how ethernet used to be before
switches?

25-15:33 &lt; rlpowell&gt; I have one Ubiquiti with perhaps 6 consumers
at any given time.

25-15:33 &lt; treed&gt; Where if one devices was talking, nothing else
could talk?

25-15:33 &lt; rlpowell&gt; 25-15:33 &lt; treed&gt; Remember how ethernet
used to be before switches? -- No.

25-15:33 &lt; treed&gt; That is also true of wireless.

25-15:33 &lt; Tene&gt; rlpowell: radio waves are a shared medium;
remember how old ethernet hubs worked with collisions?

25-15:33 &lt; treed&gt; Oh, well, that's how it used to be.

25-15:33 &lt; rlpowell&gt; 'k.

25-15:33 &lt; treed&gt; One device could talk at a time

25-15:33 &lt; treed&gt; if something else tried to talk, they'd both
bail out and wait

25-15:33 &lt; rlpowell&gt; I guess I vaaaaaaaguely remember that.

25-15:34 &lt; treed&gt; \~g CSMA-CD

25-15:34 &lt; Sexymans&gt; treed:
https://en.wikipedia.org/wiki/Carrier\_sense\_multiple\_access\_with\_collision\_detection

25-15:34 &lt; treed&gt; That's what ethernet used to use

25-15:34 &lt; treed&gt; wifi uses CSMA-CA, but it's similar

25-15:34 &lt; treed&gt; Only one thing can be talking on a shared medium
at the same time

25-15:34 &lt; treed&gt; otherwise you get interference

25-15:34 &lt; treed&gt; think about being in the car on like AM radio

25-15:35 &lt; treed&gt; and how if you're in between the locations of
the radio stations, you'll get one or the other depending on like...
where trees are and shit

25-15:35 &lt; treed&gt; whichever one is being blocked less gets to you

25-15:35 &lt; treed&gt; this applies to all radio

25-15:35 &lt; rlpowell&gt; I had no idea that wifi counted as a shared
medium in this sense.

25-15:35 &lt; treed&gt; which is one reason the FCC regulates the
airwaves

25-15:35 &lt; rlpowell&gt; 25-15:35 &lt; treed&gt; and how if you're in
between the locations of the radio stations, -- Of two radio stations on
the same frequency, you mean.

25-15:36 &lt; treed&gt; if everyone could just shit out anything they
want on any frequency, you'd get... well you'd get wifi

25-15:36 &lt; treed&gt; rlpowell: Yes.

25-15:36 &lt; rlpowell&gt; OK.

25-15:36 &lt; treed&gt; Radio, TV, and wifi are all on the same EM
spectrum

25-15:36 &lt; treed&gt; just different places

25-15:36 &lt; rlpowell&gt; So given that I *don't* want to spend \$1000
on my home wifi, what do you suggest?

25-15:36 &lt; treed&gt; so the same physics apply to all of them, if a
little differently depending on their wavelength

25-15:36 &lt; treed&gt; rlpowell: Best practical case is three APs on
channels 1, 6, 11

25-15:36 &lt; rlpowell&gt; I have a ubiquiti, a bunch of WRT-54GLs, and
this trendnet thing, but *exactly one* place that the ethernet cable can
go to the upstairs.

25-15:36 &lt; treed&gt; spread out

25-15:37 &lt; rlpowell&gt; And all the consumers are upstairs.

25-15:37 &lt; treed&gt; and wire everything that you can

25-15:37 &lt; treed&gt; so send that one cable up to a switch

25-15:37 &lt; treed&gt; and then spread it out from there

25-15:37 &lt; rlpowell&gt; Yeah, basically I can't wire anything at all.

25-15:37 &lt; rlpowell&gt; There's no way to get the cables from RA's
office to anywhere else.

25-15:37 &lt; rlpowell&gt; Come over for B5 and I'll show you what I
mean. :P

25-15:38 &lt; rlpowell&gt; Like, it's certainly *possible*, but *ugh*.

25-15:38 &lt; rlpowell&gt; So we've been relying on wifi.

25-15:38 &lt; treed&gt; other option: Point-to-point high-bandwidth link
between the two floors

25-15:38 &lt; treed&gt; and then ethernet from there

25-15:38 &lt; treed&gt; when IMVU was split between two buildings we had
two pairs of point-to-point wifi connecting the buildings

25-15:38 &lt; treed&gt; with each side having its own ethernet network

25-15:38 &lt; rlpowell&gt; So the router is in the basement, and we run
ethernet to RA's office.

25-15:39 &lt; rlpowell&gt; Everything else is wifi. In particular, the
living room, RA's laptop, RJ's laptop, and my room.

25-15:39 &lt; treed&gt; So you have an upper floor ethernet network

25-15:39 &lt; treed&gt; and then do a PTP link that *only* bridges the
upper network to the lower network

25-15:39 &lt; rlpowell&gt; No, that's the thing; there's no sane way to
get cables from RA's office to anywhere else.

25-15:39 &lt; treed&gt; Yes

25-15:39 &lt; treed&gt; So put it on the ceiling of her office

25-15:39 &lt; treed&gt; Or is that on the top floor?

25-15:39 &lt; rlpowell&gt; Which, the WAP?

25-15:39 &lt; rlpowell&gt; her office is on the top floor.

25-15:39 &lt; treed&gt; Two WAPs

25-15:39 &lt; treed&gt; One on either side of a surface

25-15:39 &lt; treed&gt; pointed at each other

25-15:40 &lt; rlpowell&gt; Huh. OK. Why?

25-15:40 &lt; treed&gt; Next best thing to drilling a hole through the
wall

25-15:40 &lt; treed&gt; You then only have one wifi link to care about

25-15:40 &lt; treed&gt; and you wire everything else

25-15:40 &lt; rlpowell&gt; I think we're miscommunicating. I'll show you
the next time you come over.

25-15:40 &lt; treed&gt; k

25-15:40 &lt; rlpowell&gt; (i.e. your responses are not relating to what
I believe I'm describing, so I'm giving up on text)

25-15:40 &lt; treed&gt; k

25-15:41 &lt; rlpowell&gt; treed: Ignoring that aspect of it, though: 3
WAPs on 1/6/11 ; would they be different SSIDs or bridged or what?

25-15:41 &lt; treed&gt; rlpowell: In general an answer to "I can't run a
cable from point A to point B" is "so make a point-to-point wireless
link"

25-15:41 &lt; treed&gt; Same SSID, and bridged.

25-15:42 &lt; treed&gt; Wifi clients will automatically get the
strongest one and switch as necessary

25-15:42 &lt; treed&gt; if they have the same SSID

25-15:42 &lt; treed&gt; and security settings

25-15:42 &lt; rlpowell&gt; treed: "point-to-point wireless link" here
means "a wap on each end with *no other job* than to talk to this
specific other thing", yes?

25-15:42 &lt; treed&gt; Yes.

25-15:42 &lt; treed&gt; This thing talks to this thing, directionally.
Nothing else talks to either thing on wireless, but they bridge two
separate segments of wired network.

25-15:42 &lt; rlpowell&gt; And that's significantly better than having a
bunch of clients on one WAP, I imagine.

25-15:43 &lt; treed&gt; You can actually buy directional antennas that
work better for this specific purpose.

25-15:43 &lt; rlpowell&gt; Because I can *do* that; I've got lie 3
WRT54GLs lying around.

25-15:43 &lt; rlpowell&gt; Oh?

25-15:43 &lt; treed&gt; Right.

25-15:43 &lt; treed&gt; Yeah, most WAP antennas are omnidirecitonal

25-15:43 &lt; treed&gt; but you can buy aftermarket antennas

25-15:43 &lt; treed&gt; including extra gain

25-15:43 &lt; rlpowell&gt; *nod*

25-15:43 &lt; rlpowell&gt; That seems like worth doing for my room, for
example.

25-15:43 &lt; treed&gt; (which may actually contravene FCC rules
depending on how you do it)

25-15:43 &lt; rlpowell&gt; Because that's the one place in the house
where the internet *has to* work, and it routinely doesn't. -\_-

25-15:44 &lt; treed&gt; But, if they're just on the other side of the
wall from each other, you can probably get away with stock antennas.

25-15:44 &lt; rlpowell&gt; hahahah no.

25-15:44 &lt; treed&gt; So if you have three extra WAPS, you have
A))\~~\~~((B-----C-))

25-15:44 &lt; rlpowell&gt; Well what i was specifically thinking of is
dedicating 2 WAPs just to my room.

25-15:45 &lt; rlpowell&gt; For each end of that connection.

25-15:45 &lt; treed&gt; where ((()) is wifi, \~\~ is a wireless
connection, and -- is wired

25-15:45 &lt; rlpowell&gt; Yeah.

25-15:45 &lt; treed&gt; or C can be a regular switch

25-15:45 &lt; treed&gt; plugged into desktops and whatnot

25-15:45 &lt; rlpowell&gt; The annoying part is that the WRT54GLs are
802.11g. :P

25-15:45 &lt; treed&gt; yeah

25-15:45 &lt; treed&gt; But a better signal at a lower speed is probably
preferable to shit signal of whatever speed.

25-15:45 &lt; rlpowell&gt; True dat.

25-15:45 &lt; treed&gt; also I've found that WRT54GLs can't do gigabit
ethernet very well either

25-15:46 &lt; rlpowell&gt; But now that I've got this trendnet thingy I
could burn that to fixing up my room.

25-15:46 &lt; rlpowell&gt; treed: I don't think anything I have does
gigabit.

25-15:46 &lt; treed&gt; wat

25-15:46 &lt; treed&gt; Is everything you own from the 90s?

25-15:46 &lt; rlpowell&gt; The reason I care about N in my room is that
sometimes we do steam in-home streaming.

25-15:47 &lt; rlpowell&gt; treed: "actually is doing gigabit in
practice"; I'm sure it's mostly capable of doing so.

25-15:47 &lt; treed&gt; Ah.

25-15:47 &lt; rlpowell&gt; My ethernet cables probably *are* from the
90s. :D

25-15:47 &lt; treed&gt; s/does/needs/

25-15:47 &lt; rlpowell&gt; I thought you needed cat 6 for gigabit?

25-15:47 &lt; rlpowell&gt; Like I didn't think gigabit would just work;
ithoughty ou needed special shit.

25-15:47 &lt; Tene&gt; Nope

25-15:48 &lt; treed&gt; nope

25-15:48 &lt; treed&gt; cat 5e is totally sufficient for gigabit

25-15:48 &lt; treed&gt; you need 6 for 10G

25-15:48 &lt; rlpowell&gt; Huh.

25-15:48 &lt; rlpowell&gt; TIL.

25-15:48 &lt; treed&gt; Yeah, gigabit is old as balls now dude

25-15:48 &lt; rlpowell&gt; Maybe I'm thinking 10G then?

25-15:48 &lt; treed&gt; Nothing should do less than gigabit.

25-15:48 &lt; treed&gt; You probably are.

25-15:48 &lt; rlpowell&gt; That would explain this conversation a lot.

25-15:48 &lt; treed&gt; Well, nothing wired should do less than gigabit.

25-15:48 &lt; treed&gt; 40Gb is a thing now

25-15:49 &lt; treed&gt; And 100Gb is coming if not already here

25-15:49 &lt; treed&gt; We just deployed 10Gb network shit at IMVU.

25-15:49 &lt; rlpowell&gt; 25-15:48 &lt; treed&gt; 40Gb is a thing now
-- Goo lord.

25-15:49 &lt; rlpowell&gt; treed: This has been a hugely informative
discussion, thank you.

25-15:49 &lt; treed&gt; NP

25-15:49 &lt; treed&gt; rlpowell: We actually use 40Gb in production too

25-15:49 &lt; treed&gt; but just for cross-connection between the two
core switches

25-15:49 &lt; rlpowell&gt; WRT interference: I guess I think of clients
as being "consumers" if you will.

25-15:49 &lt; treed&gt; I think it's actually a pair of 40Gb links

25-15:50 &lt; treed&gt; rlpowell: That would be true if you never had to
send data.

25-15:50 &lt; treed&gt; TV/radio are definitely producer/consumer

25-15:50 &lt; treed&gt; but all wifi stations talk

25-15:50 &lt; rlpowell&gt; Going to expand on that in a minute.

25-15:54 &lt; rlpowell&gt; So I guess my intuitive sense is that each
client is talking seperately to the WAP?

25-15:54 &lt; rlpowell&gt; Because they're each in a different
direction.

25-15:54 &lt; treed&gt; Ah.

25-15:54 &lt; rlpowell&gt; But what I'm hearing you say is that
whichever one is talking loudest wins because physics.

25-15:54 &lt; treed&gt; That's kinda how it works with MIMO.

25-15:55 &lt; rlpowell&gt; I'm visualizing a pool of water and imagining
signalling someone by making waves, and it's fairly obvious there that
if two of us are talking at once we're fucked.

25-15:55 &lt; treed&gt; MIMO is a thing that became available as of N,
where it'll actually have multiple distinct antennas for talking to
different devices.

25-15:55 &lt; treed&gt; Yup.

25-15:55 &lt; rlpowell&gt; Yeah, that ... explains a lot.

25-15:55 &lt; rlpowell&gt; So like my dropbox, which is a
constant-communication sort of thing, could fuck other people's ability
to do anything just because it keeps talking all the time.

25-15:56 &lt; treed&gt; Everything is talking all the time, but yeah.

25-15:56 &lt; rlpowell&gt; And TCP is bi-directional, so that alone
could explain why it all goes to shit when the girls watch something on
netflix.

25-15:56 &lt; treed&gt; CSMA-CA means that the wifi card will be
watching

25-15:56 &lt; treed&gt; trying to wait for the pool to be still

25-15:56 &lt; rlpowell&gt; Like we were doing OK-ish and then you added
one more talker and suddenly the pool is *never* still.

25-15:56 &lt; treed&gt; so it'll buffer up a little bit and then send

25-15:56 &lt; treed&gt; so it's not as bad as it could be

25-15:56 &lt; treed&gt; (The CA there stands for "carrier avoidance", as
opposed to the mere "carrier detection" that 802.3 (ethernet) did)

25-15:57 &lt; rlpowell&gt; You get that this convo implies that
basically everyone is doing wifi wrong, yeah?

25-15:57 &lt; treed&gt; Yup.

25-15:57 &lt; rlpowell&gt; Like every office I've ever been in has 20:1
or more wifi ratios.

25-15:57 &lt; rlpowell&gt; (why the fuck do my fingers keep typing
"rations"? :P )

25-15:57 &lt; treed&gt; This also explains why my apartment has the best
goddamn wifi.

25-15:57 &lt; treed&gt; I get 80Mbit to my couch.

25-15:57 &lt; rlpowell&gt; :D

25-15:57 &lt; treed&gt; or more

25-15:57 &lt; treed&gt; I can pull from steam at 11-12MB/sec

25-15:57 &lt; treed&gt; It'll actually be better soon too

25-15:58 &lt; treed&gt; Since I got my SRX-240 configured and running, I
can now move the switch I had in my closet out to the TV stand

25-15:58 &lt; treed&gt; and wire up the PS4, TV, and Chromecast

25-15:58 &lt; treed&gt; which removes three things from my wifi

25-15:58 &lt; rlpowell&gt; *nod*

25-15:58 &lt; treed&gt; Regarding 20:1 on office wifi: Yuuuuuup

25-15:58 &lt; treed&gt; Notably

25-15:58 &lt; treed&gt; When I'm at home, sitting around doing nothing,
my phone uses like no power

25-15:58 &lt; treed&gt; but a day of sitting around doing nothing at the
office will eat like half the battery

25-15:58 &lt; treed&gt; because it has to use so much more power on the
wifi

25-15:58 &lt; rlpowell&gt; Because it's spending all its time fi.. yeah.

25-15:59 &lt; rlpowell&gt; Fuck I have learned so much in this
conversation. :D

25-15:59 &lt; treed&gt; :D

25-15:59 &lt; treed&gt; So, I lucked out when I got my current place

25-15:59 &lt; rlpowell&gt; Do you mind if I save it off somewhere
web-accessible for future editing?

25-15:59 &lt; rlpowell&gt; (i.e. do you care if this is on the web
verbatim for now?)

25-15:59 &lt; treed&gt; it actually has a switching panel in the closet
with 4 ethernet jacks that go to different places

25-15:59 &lt; treed&gt; Nah, go for it.
