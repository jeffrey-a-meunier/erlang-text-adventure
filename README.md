# Erlang text adventure

This is a very simple text adventure game. The goal is to find the treasure.

The file `setup.erl` contains the database setup code, and all the spoilers.

The program is interesting because much of it is controlled by triggers. The
triggers are used to do things like display a message to the user, create a
new access to a location, or prevent access to a location until the user uses
a key. A trigger can be set to modify any table in the database in any way.

Pre-triggers are checked after the user enters a command, but before the command
action action is performed. If the pre-trigger returns a successful result, then
the command is executed. Post-triggers are checked only if the command finished
successfully.

1. User enters command.
2. Activate pre-triggers.
3. If pre-trigger successful, then:
4. Do the command.
5. Activate post-triggers.

## 1. Install Erlang


```
$ apt install erlang
```

## 2. Compile the source code

From bash:

```
$ erlc *.erl
```

From within erlang:

```
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:1:1] [ds:1:1:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> make:all().
Recompile: cmd
Recompile: db
Recompile: location
Recompile: main
Recompile: parser
Recompile: player
Recompile: setup
Recompile: trigger
Recompile: utils
up_to_date
2>
```

Note that a statement in Erlang must be terminated with a full-stop (period) '.'.

## 3. Run the game

From bash:

```
$ erl -noshell -s main -s erlang halt
```

From within Erlang:

```
2> main:start().
```

Don't forget the period!
