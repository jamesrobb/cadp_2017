# Code structure

## Files

The files in of the project are as follows, 

* ebin
  * polyverse.app
  * *.beam
  * storage1
* src
  * polyverse.erl
  * polyverse_serv.erl
  * polyverse_super.erl
  * polyverse_transfer
* Emakefile

## ebin
### polyverse.app

When you want to use the Polyverse software you must generate a gpg private key and store it inside this **polyverse.app** file. This key must be stored inside the environment variables under **gpg_id**.

Example:

```
{application, polyverse,
	[{vsn, "0.1"},
	{modules, [polyverse]},
	{mod, {polyverse, []}},
	{env, [{storage_directory, './storage1/'},
		   {gpg_id, '77269...'}
	]
}.
```

### storage1

This is the folder provided in the **polyverse.app** file denoting where the encrypted files stored on Polyverse will be stored in local storage.

## src

### polyverse.erl

This file is erlang's generic application file. It describes how the application start and stop and the functions described in it are the functions you will be able to call when running the application.

### polyverse_serv.erl

This file is the generic server behaviour of our application. In this file is where most of the logic goes on. Whenever the application receives an erlang message that matches a call that the generic server expects a **handle_call()** function will be called. All of the handle_call functions are described in this file.

### polyverse_super.erl

This file is the generic supervisor behaviour of our application. In this file we describe how the system should react to a crash of the polyverse_serv process.

### polyverse_transfer.erl

In this file much of the business logic that does not have anything to do with handling of calls is. In this file the functions that encrypt, decrypt, send, and receive files are realized.


# How to compile and run the software

## Compiling

To compile the software you MUST be in the directory where **Emakefile** is located. In the case of Polyverse it is located in the root of the Polyverse folder.

To compile the software run the command 

`erl -make`

## Starting

When you want to start a node which will then connect to a network you MUST be within the /ebin/ directory of the project and run the command:

`erl -sname NAME@localhost -setcookie COOKIE` 

Where NAME is the name you wish your node to have, and COOKIE is the cookie of the network you wish to connect to. If you're running the system locally there is no requirement for a cookie to be set, just make sure that you put @localhost on all nodes running locally.

After you've started a node you MUST begin by starting the Polyverse application. This can be done by running the command:

`application:start(polyverse).`

## Connecting

After you've gotten the Polyverse application running you can connect to an existing network with the command:

`polyverse:connect_to_node(NODE)`
 
 Where NODE is a node inside the network you would like to connect to. If you can not connect two primary causes can be the issue. Those causes are, the cookie you set when initially starting your node is not the same cookie as the cookie for the network, or the port used to connect the two nodes might be blocked (if you're inside RU they are, but connecting withing the same computer will work fine).


## Commands inside the system

Polyverse has multiple commands you're able to run when you want to play around in the system. Those commands include

### Adding files

`polyverse:add_file(FILE)`

Where FILE is the path to a file you wish to add to the Polyverse network you're currently connected to.

### Requesting Files

`polyverse:get_file(FILE)`

Where FILE the hash digest of a file currently on the Polyverse network.

### List Files

`polyverse:get_file_list()`

Prints a list of hash digest you have in your local file storage.

### Decrypting files

To decrypt files you can run the command:

`polyverse:decrypt_file(DIGEST, OUTPUTFILE)`

Where DIGEST is a string of a filename inside your local storage **storage1/**, and where OUTPUTFILE is a string that is the filename you wish the decrypted file should be stored in. The stored file will be stored in your **ebin/** folder.

You can also go the manual route by running this command:

`gpg --ouput OUTPUTFILE --decrypt ENCRYPTEDFILE`

Where OUTPUTFILE is the file you wish to decrypt your file into, and ENCRYPTEDFILE is a file inside **storage1**.

# Usage scenarios

## General use case

When starting to use Polyverse the first thing to do is to generate a gpg private key or use an existing one, to do this you can run the command:

`gpg --gen-key`

After you've done that you can look at your private keys by running the command:

`gpg --list-secret-keys`

Now you can add the secret key from there into your **polyverse.app** file as described earlier.

If you've already created **storage1/** folder in the place specified earlier otherwise create it now. Now you're ready to start the Polyverse application. 

You should now be able to follow the commands in the section **How to compile and run the software** to play around with this system.


