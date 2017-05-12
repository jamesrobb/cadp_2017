# How to compile and run the software

## Compiling

To compile the software you MUST be in the directory where **Emakefile** is located. In the case of Polyverse it is located in the root of the Polyverse folder.

To compile the software run the command 

`erl -make`

## Starting

When you want to start a node which will then connect to a network you MUST be within the /ebin/ directory of the project and run the command:

`erl -sname "somename"@localhost -setcookie "cookie"` 

the "somename" can be replaced with the name you wish your node to have. The "cookie" can be replaced with the cookie of the network you wish to connect to. During the demostration this cookie will be named "mandm".

After you've started a node you MUST begin by starting the Polyverse application. This can be done by running the command:

`Application:start(polyverse).`

## Connecting

After you've gotten the Polyverse application running you can connect to an existing network with the command:

`polyverse:connect_to_node("Node")`
 
 Where Node is a node inside the network you would like to connect to. If you can not connect two primary causes can be the issue. Those causes are, the cookie you set when initially starting your node is not the same cookie as the cookie for the network, or the port used to connect the two nodes might be blocked (if you're inside RU they are).


## Commands inside the system

Polyverse has multiple commands you're able to run when you want to play around in the system. Those commands include

### Adding files

`polyverse:add_file(FILE)`

Where FILE is the path to a file you wish to add to the Polyverse network you're currently connected to.

### Requesting Files

`polyverse:get_file(FILE)`

Where FILE the hash digest of a file currently on the Polyverse network.


