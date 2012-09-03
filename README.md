FixTestErl
==========

FixTestErl is a system for automated testing and market simultion. 
It allows users to test positive and negative use-case scenarios 
within buy-side and sell-side applications, which speak FIX. 
FixTestErl supports cross session integration giving the ability 
to complete robust testing across systems. Multi-session capabilities 
allows simulation of many counterparties at once. Additionally FixTestErl 
can be used for high-speed testing.



To build and install


1. Get and install an Erlang system (http://www.erlang.org)

2.  Clone the source from github:
# git clone https://github.com/MaximMinin/FixTestErl.git

3. Install dependencies usung rebar:
# ./rebar get-deps

4. Build using rebar:
# ./rebar compile
# ./rebar generate

5. To test the build, start it as
# ./rel/bin/fixTestErl console
This will be start an interactive system with erlang shell.
FixTestErl can be start as background proces too:
# ./rel/bin/fixTestErl start
6. The administratio web frontend will be created at http://${host}:$port/index.yaws

