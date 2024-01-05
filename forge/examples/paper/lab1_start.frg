#lang forge

/* ====================================================================
A SIMPLIFIED MODEL OF NETWORK STATE 

  Adapted for use in lab 1, FMSS23
  Pamela Zave and Tim Nelson
  Starter model and template
    for use _after_ the initial intro-to-reachability collab livecode

  Further adapted by Tim, converting from Alloy to Forge. 
    -- removed util/ordering[MName]

==================================================================== */
/* --------------------------------------------------------------------
TYPES
-------------------------------------------------------------------- */


sig MName { }                            -- the MName of a network member

-- TN: used to be a "top" for LinkIdent, Self, Command
abstract sig Value {}
sig LinkIdent extends Value {}
sig SessionIdent { }
 
abstract sig Command extends Value { }
   one sig Receive, Drop extends Command { }
one sig Self extends Value { }

sig NetHdr
   {  src, dst: one MName, sessionIdent: one SessionIdent  }
pred NetHdrsAreRecords { all h, h": NetHdr |
   (  h.sessionIdent = h".sessionIdent 
   && h.src = h".src && h.dst = h".dst  ) 
   => h = h" }           -- if an object is a "record," the identity of
                         -- an instance depends only on its field values

sig Link { 
   sndr: one MName, 
   sndrIdent: one LinkIdent,
   rcvr: one MName,
   rcvrIdent: one LinkIdent
} 
-- Note that links are not records; there can be two distinct 
-- individuals that happen to have the same field values.
   
sig ForwardTable { frows:
   set Value -> NetHdr -> Value  }

/* --------------------------------------------------------------------
NETWORK STATES
   There are many versions of reachable here, only one of which is
usable. For pedagogical purposes, the MNames must be obscured!
-------------------------------------------------------------------- */

--  "Name" -> "MName" (hash)

sig NetworkState {
-- Network components (domain knowledge).
   members: set MName,        -- every member has at least a unique MName
   infras: set MName,   -- trusted and untrusted members
   users: set MName,    -- trusted and untrusted members
   links: set Link,
   outLinks, inLinks: set MName -> LinkIdent -> Link,         -- derived
-- Network traffic (domain knowledge).
   sendTable: set MName -> NetHdr,
   receiveTable: set MName -> NetHdr,
-- Behavior of network members (specification).
   forwardTables: pfunc MName -> ForwardTable,
   oneStep: set NetHdr -> Link -> Link,              -- all derived below
-- Four competing definitions of reachability in this model
   reachable1: set NetHdr -> MName -> MName,
   reachable2: set NetHdr -> MName -> MName,
   reachable3: set NetHdr -> MName -> MName,
   reachable4: set NetHdr -> MName -> MName
}  

pred NetworkState_facts {
   all ns: NetworkState | {

   no ns.infras & ns.users

   ns.users in ns.members
    ns.infras in ns.members
    ns.outLinks in ns.members -> LinkIdent -> ns.links
    ns.inLinks in ns.members -> LinkIdent -> ns.links
    ns.sendTable in ns.members -> NetHdr
    ns.receiveTable in ns.members -> NetHdr
    ns.forwardTables in ns.members -> ForwardTable 
    ns.oneStep in NetHdr -> ns.links -> ns.links


-- Network topology.
   -- Each member is an infrastructure member or user.
      ns.infras + ns.users = ns.members
   -- All links have at least one endpoint that is a member.
      all k: ns.links | some ((k.sndr + k.rcvr) & ns.members) 
   -- There are no self-links.
      all k: ns.links | k.sndr != k.rcvr
   -- For each member, its local linkIdents are disjoint.
      all disj k0, k1: ns.links | 
      (   (k0.sndr = k1.sndr => k0.sndrIdent != k1.sndrIdent)
      &&  (k0.rcvr = k1.rcvr => k0.rcvrIdent != k1.rcvrIdent)
      &&  (k0.sndr = k1.rcvr => k0.sndrIdent != k1.rcvrIdent)  )
   -- Derivation of inLinks and outLinks.
      ns.outLinks = { m: ns.members, i: LinkIdent, k: ns.links |
         k.sndr = m && k.sndrIdent = i                }         
      ns.inLinks = { m: ns.members, i: LinkIdent, k: ns.links |
         k.rcvr = m && k.rcvrIdent = i               }
-- Behavior of network members.  
   -- The forwardTable.  The absence of a matching table entry means 
   -- that an incoming packet is dropped.
      -- Consistency: All inLinks and outLinks are real.
      all m: ns.members | all kin: 
         (((m.(ns.forwardTables)).frows).(LinkIdent + Command)).NetHdr |
         ( (m -> kin) in (ns.inLinks).(ns.links) || kin in Self )

      all m: ns.members | all kout:
              NetHdr.((LinkIdent + Self).((m.(ns.forwardTables)).frows)) |
         kout ! in Command => (m -> kout) in (ns.outLinks).(ns.links)
      -- A forwardTable can yield multiple outLinks for replication for
      -- allCast service.  In addition to yielding multiple outLinks, 
      -- it can yield a Receive command.  Otherwise it is 
      -- deterministic.  If there is no matching entry for an incoming
      -- packet, it is dropped.
         all m: ns.members | all ki: LinkIdent + Command, h: NetHdr |
            (  lone h.(ki.((m.(ns.forwardTables)).frows))
            || h.(ki.((m.(ns.forwardTables)).frows)) in 
                  (LinkIdent + Receive)            )
-- Reachability.
      ns.oneStep = {  h: NetHdr, k, k": ns.links | k != k" && {
         some m: MName, kin, kout: LinkIdent |
            (m -> kin -> k) in ns.inLinks 
         && (m -> kout -> k") in ns.outLinks
         && (kin -> h -> kout) in (m.(ns.forwardTables)).frows  }}

      ns.reachable1 = {  h: NetHdr, m, m": MName | 
         some k, k": ns.links | 
            m in k.sndr && m" in k".rcvr && (k -> k") in ^(h.(ns.oneStep)) }
      ns.reachable2 = {  h: NetHdr, m, m": MName | 
         some k, k": ns.links | 
            m in k.sndr && m" in k".rcvr 
         && (  k = k" || (k -> k") in ^(h.(ns.oneStep))  )          }
      ns.reachable3 = {  h: NetHdr, m, m": MName | 
         some k, k": ns.links, kin, kout: LinkIdent | 
            (m -> kout -> k) in ns.outLinks
         && (m" -> kin -> k") in ns.inLinks
         && (h -> kout) in 
               (LinkIdent + Self).((m.(ns.forwardTables)).frows)
         && (kin -> h) in 
               ((m".(ns.forwardTables)).frows).(LinkIdent + Receive)
         && (  k = k" || (k -> k") in ^(h.(ns.oneStep))  )                }
      ns.reachable4 = {  h: NetHdr, m, m": MName | 
         some k, k": ns.links, kin, kout: LinkIdent | 
            (m -> kout -> k) in ns.outLinks
         && (m" -> kin -> k") in ns.inLinks
         && (h -> kout) in Self.((m.(ns.forwardTables)).frows)
         && (kin -> h) in ((m".(ns.forwardTables)).frows).Receive
         && (  k = k" || (k -> k") in ^(h.(ns.oneStep)) )      }

}}


/* --------------------------------------------------------------------
SANITY CHECKS ON SPECIALIZED NETWORKS

In practice, there might be many such test networks, along with many 
property checks. This template has been simplified for brevity.

-------------------------------------------------------------------- */
 
one sig m0, m1, m2, m3 extends MName {}
one sig k01, k12, k23 extends Link {}
one sig j0, j1 extends LinkIdent {}  
one sig hdr, hdrB, hdrC extends NetHdr {}
-- TN note atom naming here

-- This predicate describes a specific network topology with 4 members,
-- along with a specific packet trajectory
pred TestNet[n: NetworkState] {
   -- members of the network and which are endpoints
   n.members = m0 + m1 + m2 + m3 && n.users = m0 + m3
   -- topology of the network
   n.links = k01 + k12 + k23
   k01.sndr = m0      && k12.sndr = m1      && k23.sndr = m2
   k01.sndrIdent = j1 && k12.sndrIdent = j1 && k23.sndrIdent = j1
   k01.rcvr = m1      && k12.rcvr = m2      && k23.rcvr = m3
   k01.rcvrIdent = j0 && k12.rcvrIdent = j0 && k23.rcvrIdent = j0

   -- seeking a packet (with header h) from m0 to m3
   hdr.src = m0 && hdr.dst = m3

   -- specific forwarding table for this example network
   -- since this is for a single packet `h`, the table only covers `h`
   -- Table at m0: `h` received from OS, send out port j1
   (m0.(n.forwardTables)).frows = (Self -> hdr -> j1)
   -- Table at m1: `h` received from port `j0`, send out port `j1`
   (m1.(n.forwardTables)).frows = (j0 -> hdr -> j1)
   -- Table at m2: `h` received from port `j0`, send out port `j1`
   (m2.(n.forwardTables)).frows = (j0 -> hdr -> j1)
   -- Table at m3: `h` received from port `j0`, handled by OS
   (m3.(n.forwardTables)).frows = (j0 -> hdr -> Receive) 

   -- Impose the natural ordering; may be useful later
   --lt [m0, m1] && lt [m1, m2] && lt [m2, m3]
}

-- ******************************************************************
-- ** TASK 1: Use the visualizer to examine how the various definitions 
--   of reachability vary for this test network. 
--   (1) Execute ---> "Run TestNet..."
--   (2) Click the "Instance" link in the pane to the right.
--       (Mac users may need to find and expand the visualization window)
--   (3) We have also provided an alternative, domain-specific visualization.
--       To access it, while viewing the default visualization:
--         (I) Click File->Export To->XML, and save the instance in a file.
--         (II) Open a web browser (that supports JavaScript) and go to:
--              https://tnelson.github.io/sterling-ts/
--         (III) In the lower-right corner, click "Manual Datum"
--         (IV) Click to add XML from file, and select the file you saved to.
--              You should see a directed graph appear.
--         (V) In the upper-right corner, click "Script"
--         (VI) We have pre-loaded the visualizer for this problem. Click the 
--              blue "Run" button at the top of the window.

run {
   some ns: NetworkState | TestNet[ns]
   NetHdrsAreRecords -- fact 
   NetworkState_facts -- fact
} 
for 1 but 
4 MName, 2 LinkIdent, 3 Link, 3 NetHdr, 4 ForwardTable










-- ******************************************************************
-- ** TASK 2: Write some behavioral predicates that might be useful.
-- **    (Recall these don't need to be true of every network.)
-- ** Write them *semi-formally*, in natural language, first. 
-- ** Then we'll fill in one formally together.





-- ******************************************************************
-- ** TASK 3: Write some assertions about TestNet
-- **    (Here, you'll want something true in every network. Hint, use implication!)
-- ** Write them *semi-formally*, in natural language, first. 
-- ** Then we'll fill in one formally together.




