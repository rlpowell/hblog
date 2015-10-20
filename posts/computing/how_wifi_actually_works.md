---
title: How WiFi Actually Works
origdate: 2015-08-25
published: 2015-08-25
---
# Note no tags

25-15:31 <       treed> In the ideal situation for wifi, you have one station per AP, on non-overlapping channels

25-15:31 <       treed> which in the best possible 2.4Ghz case gets you 3 stations

25-15:31 <       treed> but the closer you can get to that ideal, the better

25-15:31 <    rlpowell> "station"?

25-15:31 <       treed> more APs and wire the fuck out of everything that can be wired

25-15:32 <       treed> generic term for "thing speaking wifi"

25-15:32 <    rlpowell> I'm sorry, are you telling me that the ideal ration of wifi consumer to WAP is 1:1 ?

25-15:32 <    rlpowell> Because if not, I'm lost.

25-15:32 <       treed> Absolutely.

25-15:32 <        Tene> rlpowell: Yes; that will get you optimal performance

25-15:32 <       treed> Having more than 1:1 means it has to share the air.

25-15:32 <       treed> And sharing means collisions.

25-15:32 <    rlpowell> OK, bear in mind that this is all magic to me.

25-15:33 <    rlpowell> It what?

25-15:33 <       treed> Remember how ethernet used to be before switches?

25-15:33 <    rlpowell> I have one Ubiquiti with perhaps 6 consumers at any given time.

25-15:33 <       treed> Where if one devices was talking, nothing else could talk?

25-15:33 <    rlpowell> 25-15:33 <       treed> Remember how ethernet used to be before switches? -- No.

25-15:33 <       treed> That is also true of wireless.

25-15:33 <        Tene> rlpowell: radio waves are a shared medium; remember how old ethernet hubs worked with collisions?

25-15:33 <       treed> Oh, well, that's how it used to be.

25-15:33 <    rlpowell> 'k.

25-15:33 <       treed> One device could talk at a time

25-15:33 <       treed> if something else tried to talk, they'd both bail out and wait

25-15:33 <    rlpowell> I guess I vaaaaaaaguely remember that.

25-15:34 <       treed> ~g CSMA-CD

25-15:34 <    Sexymans> treed: https://en.wikipedia.org/wiki/Carrier_sense_multiple_access_with_collision_detection

25-15:34 <       treed> That's what ethernet used to use

25-15:34 <       treed> wifi uses CSMA-CA, but it's similar

25-15:34 <       treed> Only one thing can be talking on a shared medium at the same time

25-15:34 <       treed> otherwise you get interference

25-15:34 <       treed> think about being in the car on like AM radio

25-15:35 <       treed> and how if you're in between the locations of the radio stations, you'll get one or the other depending on like... where trees are and shit

25-15:35 <       treed> whichever one is being blocked less gets to you

25-15:35 <       treed> this applies to all radio

25-15:35 <    rlpowell> I had no idea that wifi counted as a shared medium in this sense.

25-15:35 <       treed> which is one reason the FCC regulates the airwaves

25-15:35 <    rlpowell> 25-15:35 <       treed> and how if you're in between the locations of the radio stations, -- Of two radio stations on the same frequency, you mean.

25-15:36 <       treed> if everyone could just shit out anything they want on any frequency, you'd get... well you'd get wifi

25-15:36 <       treed> rlpowell: Yes.

25-15:36 <    rlpowell> OK.

25-15:36 <       treed> Radio, TV, and wifi are all on the same EM spectrum

25-15:36 <       treed> just different places

25-15:36 <    rlpowell> So given that I *don't* want to spend $1000 on my home wifi, what do you suggest?

25-15:36 <       treed> so the same physics apply to all of them, if a little differently depending on their wavelength

25-15:36 <       treed> rlpowell: Best practical case is three APs on channels 1, 6, 11

25-15:36 <    rlpowell> I have a ubiquiti, a bunch of WRT-54GLs, and this trendnet thing, but *exactly one* place that the ethernet cable can go to the upstairs.

25-15:36 <       treed> spread out

25-15:37 <    rlpowell> And all the consumers are upstairs.

25-15:37 <       treed> and wire everything that you can

25-15:37 <       treed> so send that one cable up to a switch

25-15:37 <       treed> and then spread it out from there

25-15:37 <    rlpowell> Yeah, basically I can't wire anything at all.

25-15:37 <    rlpowell> There's no way to get the cables from RA's office to anywhere else.

25-15:37 <    rlpowell> Come over for B5 and I'll show you what I mean.  :P

25-15:38 <    rlpowell> Like, it's certainly *possible*, but *ugh*.

25-15:38 <    rlpowell> So we've been relying on wifi.

25-15:38 <       treed> other option: Point-to-point high-bandwidth link between the two floors

25-15:38 <       treed> and then ethernet from there

25-15:38 <       treed> when IMVU was split between two buildings we had two pairs of point-to-point wifi connecting the buildings

25-15:38 <       treed> with each side having its own ethernet network

25-15:38 <    rlpowell> So the router is in the basement, and we run ethernet to RA's office.

25-15:39 <    rlpowell> Everything else is wifi.  In particular, the living room, RA's laptop, RJ's laptop, and my room.

25-15:39 <       treed> So you have an upper floor ethernet network

25-15:39 <       treed> and then do a PTP link that *only* bridges the upper network to the lower network

25-15:39 <    rlpowell> No, that's the thing; there's no sane way to get cables from RA's office to anywhere else.

25-15:39 <       treed> Yes

25-15:39 <       treed> So put it on the ceiling of her office

25-15:39 <       treed> Or is that on the top floor?

25-15:39 <    rlpowell> Which, the WAP?

25-15:39 <    rlpowell> her office is on the top floor.

25-15:39 <       treed> Two WAPs

25-15:39 <       treed> One on either side of a surface

25-15:39 <       treed> pointed at each other

25-15:40 <    rlpowell> Huh.  OK.  Why?

25-15:40 <       treed> Next best thing to drilling a hole through the wall

25-15:40 <       treed> You then only have one wifi link to care about

25-15:40 <       treed> and you wire everything else

25-15:40 <    rlpowell> I think we're miscommunicating.  I'll show you the next time you come over.

25-15:40 <       treed> k

25-15:40 <    rlpowell> (i.e. your responses are not relating to what I believe I'm describing, so I'm giving up on text)

25-15:40 <       treed> k

25-15:41 <    rlpowell> treed: Ignoring that aspect of it, though: 3 WAPs on 1/6/11 ; would they be different SSIDs or bridged or what?

25-15:41 <       treed> rlpowell: In general an answer to "I can't run a cable from point A to point B" is "so make a point-to-point wireless link"

25-15:41 <       treed> Same SSID, and bridged.

25-15:42 <       treed> Wifi clients will automatically get the strongest one and switch as necessary

25-15:42 <       treed> if they have the same SSID

25-15:42 <       treed> and security settings

25-15:42 <    rlpowell> treed: "point-to-point wireless link" here means "a wap on each end with *no other job* than to talk to this specific other thing", yes?

25-15:42 <       treed> Yes.

25-15:42 <       treed> This thing talks to this thing, directionally. Nothing else talks to either thing on wireless, but they bridge two separate segments of wired network.

25-15:42 <    rlpowell> And that's significantly better than having a bunch of clients on one WAP, I imagine.

25-15:43 <       treed> You can actually buy directional antennas that work better for this specific purpose.

25-15:43 <    rlpowell> Because I can *do* that; I've got lie 3 WRT54GLs lying around.

25-15:43 <    rlpowell> Oh?

25-15:43 <       treed> Right.

25-15:43 <       treed> Yeah, most WAP antennas are omnidirecitonal

25-15:43 <       treed> but you can buy aftermarket antennas

25-15:43 <       treed> including extra gain

25-15:43 <    rlpowell> *nod*

25-15:43 <    rlpowell> That seems like worth doing for my room, for example.

25-15:43 <       treed> (which may actually contravene FCC rules depending on how you do it)

25-15:43 <    rlpowell> Because that's the one place in the house where the internet *has to* work, and it routinely doesn't.  -_-

25-15:44 <       treed> But, if they're just on the other side of the wall from each other, you can probably get away with stock antennas.

25-15:44 <    rlpowell> hahahah no.

25-15:44 <       treed> So if you have three extra WAPS, you have A))~~~~((B-----C-))

25-15:44 <    rlpowell> Well what i was specifically thinking of is dedicating 2 WAPs just to my room.

25-15:45 <    rlpowell> For each end of that connection.

25-15:45 <       treed> where ((()) is wifi, ~~ is a wireless connection, and -- is wired

25-15:45 <    rlpowell> Yeah.

25-15:45 <       treed> or C can be a regular switch

25-15:45 <       treed> plugged into desktops and whatnot

25-15:45 <    rlpowell> The annoying part is that the WRT54GLs are 802.11g.  :P

25-15:45 <       treed> yeah

25-15:45 <       treed> But a better signal at a lower speed is probably preferable to shit signal of whatever speed.

25-15:45 <    rlpowell> True dat.

25-15:45 <       treed> also I've found that WRT54GLs can't do gigabit ethernet very well either

25-15:46 <    rlpowell> But now that I've got this trendnet thingy I could burn that to fixing up my room.

25-15:46 <    rlpowell> treed: I don't think anything I have does gigabit.

25-15:46 <       treed> wat

25-15:46 <       treed> Is everything you own from the 90s?

25-15:46 <    rlpowell> The reason I care about N in my room is that sometimes we do steam in-home streaming.

25-15:47 <    rlpowell> treed: "actually is doing gigabit in practice"; I'm sure it's mostly capable of doing so.

25-15:47 <       treed> Ah.

25-15:47 <    rlpowell> My ethernet cables probably *are* from the 90s. :D

25-15:47 <       treed> s/does/needs/

25-15:47 <    rlpowell> I thought you needed cat 6 for gigabit?

25-15:47 <    rlpowell> Like I didn't think gigabit would just work; ithoughty ou needed special shit.

25-15:47 <        Tene> Nope

25-15:48 <       treed> nope

25-15:48 <       treed> cat 5e is totally sufficient for gigabit

25-15:48 <       treed> you need 6 for 10G

25-15:48 <    rlpowell> Huh.

25-15:48 <    rlpowell> TIL.

25-15:48 <       treed> Yeah, gigabit is old as balls now dude

25-15:48 <    rlpowell> Maybe I'm thinking 10G then?

25-15:48 <       treed> Nothing should do less than gigabit.

25-15:48 <       treed> You probably are.

25-15:48 <    rlpowell> That would explain this conversation a lot.

25-15:48 <       treed> Well, nothing wired should do less than gigabit.

25-15:48 <       treed> 40Gb is a thing now

25-15:49 <       treed> And 100Gb is coming if not already here

25-15:49 <       treed> We just deployed 10Gb network shit at IMVU.

25-15:49 <    rlpowell> 25-15:48 <       treed> 40Gb is a thing now -- Goo lord.

25-15:49 <    rlpowell> treed: This has been a hugely informative discussion, thank you.

25-15:49 <       treed> NP

25-15:49 <       treed> rlpowell: We actually use 40Gb in production too

25-15:49 <       treed> but just for cross-connection between the two core switches

25-15:49 <    rlpowell> WRT interference: I guess I think of clients as being "consumers" if you will.

25-15:49 <       treed> I think it's actually a pair of 40Gb links

25-15:50 <       treed> rlpowell: That would be true if you never had to send data.

25-15:50 <       treed> TV/radio are definitely producer/consumer

25-15:50 <       treed> but all wifi stations talk

25-15:50 <    rlpowell> Going to expand on that in a minute.

25-15:54 <    rlpowell> So I guess my intuitive sense is that each client is talking seperately to the WAP?

25-15:54 <    rlpowell> Because they're each in a different direction.

25-15:54 <       treed> Ah.

25-15:54 <    rlpowell> But what I'm hearing you say is that whichever one is talking loudest wins because physics.

25-15:54 <       treed> That's kinda how it works with MIMO.

25-15:55 <    rlpowell> I'm visualizing a pool of water and imagining signalling someone by making waves, and it's fairly obvious there that if two of us are talking at once we're fucked.

25-15:55 <       treed> MIMO is a thing that became available as of N, where it'll actually have multiple distinct antennas for talking to different devices.

25-15:55 <       treed> Yup.

25-15:55 <    rlpowell> Yeah, that ... explains a lot.

25-15:55 <    rlpowell> So like my dropbox, which is a constant-communication sort of thing, could fuck other people's ability to do anything just because it keeps talking all the time.

25-15:56 <       treed> Everything is talking all the time, but yeah.

25-15:56 <    rlpowell> And TCP is bi-directional, so that alone could explain why it all goes to shit when the girls watch something on netflix.

25-15:56 <       treed> CSMA-CA means that the wifi card will be watching

25-15:56 <       treed> trying to wait for the pool to be still

25-15:56 <    rlpowell> Like we were doing OK-ish and then you added one more talker and suddenly the pool is *never* still.

25-15:56 <       treed> so it'll buffer up a little bit and then send

25-15:56 <       treed> so it's not as bad as it could be

25-15:56 <       treed> (The CA there stands for "carrier avoidance", as opposed to the mere "carrier detection" that 802.3 (ethernet) did)

25-15:57 <    rlpowell> You get that this convo implies that basically everyone is doing wifi wrong, yeah?

25-15:57 <       treed> Yup.

25-15:57 <    rlpowell> Like every office I've ever been in has 20:1 or more wifi ratios.

25-15:57 <    rlpowell> (why the fuck do my fingers keep typing "rations"? :P )

25-15:57 <       treed> This also explains why my apartment has the best goddamn wifi.

25-15:57 <       treed> I get 80Mbit to my couch.

25-15:57 <    rlpowell> :D

25-15:57 <       treed> or more

25-15:57 <       treed> I can pull from steam at 11-12MB/sec

25-15:57 <       treed> It'll actually be better soon too

25-15:58 <       treed> Since I got my SRX-240 configured and running, I can now move the switch I had in my closet out to the TV stand

25-15:58 <       treed> and wire up the PS4, TV, and Chromecast

25-15:58 <       treed> which removes three things from my wifi

25-15:58 <    rlpowell> *nod*

25-15:58 <       treed> Regarding 20:1 on office wifi: Yuuuuuup

25-15:58 <       treed> Notably

25-15:58 <       treed> When I'm at home, sitting around doing nothing, my phone uses like no power

25-15:58 <       treed> but a day of sitting around doing nothing at the office will eat like half the battery

25-15:58 <       treed> because it has to use so much more power on the wifi

25-15:58 <    rlpowell> Because it's spending all its time fi.. yeah.

25-15:59 <    rlpowell> Fuck I have learned so much in this conversation.  :D

25-15:59 <       treed> :D

25-15:59 <       treed> So, I lucked out when I got my current place

25-15:59 <    rlpowell> Do you mind if I save it off somewhere web-accessible for future editing?

25-15:59 <    rlpowell> (i.e. do you care if this is on the web verbatim for now?)

25-15:59 <       treed> it actually has a switching panel in the closet with 4 ethernet jacks that go to different places

25-15:59 <       treed> Nah, go for it.
