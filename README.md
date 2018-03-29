# cre
###### Common runtime environment for distributed programming languages

[![hex.pm](https://img.shields.io/hexpm/v/cre.svg?style=flat-square)](https://hex.pm/packages/cre) [![Build Status](https://travis-ci.org/joergen7/cre.svg?branch=master)](https://travis-ci.org/joergen7/cre)

The common runtime environment (CRE) is a generic distributed execution environment for programming languages implemented in [distributed Erlang](https://www.erlang.org). It is responsible for managing communication with a client running a language interpreter and several distributed worker processes. Herein, the CRE performs scheduling, client and worker failure recovery, and caching. The CRE is language-independent. To specialize it towards a particular distributed programming language, e.g., [Cuneiform](https://cuneiform-lang.org/), the `cre_client` and `cre_worker` behaviors must be implemented. The CRE is based on the [gen_pnet](https://github.com/joergen7/gen_pnet) OTP behavior for modeling concurrent systems as Petri nets.


![cre Petri net model](priv/cre_master_pnet.png)

*Figure 1: Petri net model of the common runtime environment's master application. It provides interfaces to client applications (top and left) and worker processes (right).*


## Features

This section gives an overview of the features, the CRE covers. The primary features of the CRE are scheduling, fault tolerance, and caching.


### Scheduling

Scheduling associates a function application to be executed with a worker process. Once the match has been made, the application is sent to the worker and the application-worker pair is ear-marked as busy. Currently, the scheduler randomly matches applications and workers, thus, maximizing the average balance of load.


### Fault Tolerance

In general, the CRE serves multiple clients and feeds multiple workers. In large setups there is a realistic chance for one or more connected processes to fail. The CRE detects such failures and appropriately reacts to them. I.e., an application sent to a failed worker is rescheduled and an application from a failed client is cached so that it is ready when the client reconnects.


### Caching

Often in data analysis applications, the storage needed to cache an intermediate results is cheaper than the compute resources needed to recompute it. Accordingly, the CRE memoizes all application-result combinations it received from workers. I.e., a computation is scheduled only if it is presented to the CRE for the first time. Later requests for the same application are served from the cache.


## Usage

Creating a distributed programming language using the CRE requires adding the CRE library to your project and implementing the callback functions for both a CRE client and a CRE worker. In this section we show, how this can be accomplished.


### Adding the CRE to a Project

Although the CRE library can be imported also directly from GitHub, we recommend adding a dependency via [hex.pm](https://hex.pm). Here, we show how this can be done using the build tools [rebar3](https://www.rebar3.org) or mix.


#### rebar3

To integrate the CRE into a rebar3 managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{cre, "0.1.5"}`.

```erlang
{deps, [{cre, "0.1.5"}]}.
```

#### mix

In an Elixir context, the CRE can be integrated into the project via mix.

```elixir
{:cre, "~> 0.1.5"}
```

### Starting the CRE Master

The CRE (its master application) can be started in four distinct ways: From the command line, as an Erlang application, under the default supervisor, and directly by spawning a CRE master process.

#### Starting from the command line

Compiling the CRE using

    rebar3 escriptize

creates an Erlang script file `cre` which allows starting the CRE via the command line. Starting the script entering

    _build/default/bin/cre

creates an Erlang node with the node name `cre@my_node` where `my_node` is the hostname of the current computer. This name is printed out on the terminal and is important for the client and worker services to connect.

Knowing the node name of the CRE, in this case `cre@mynode`, you can find out the process id of the CRE from a remote Erlang instance by calling

```erlang
cre:pid( 'cre@my_node' ).
```

The Erlang instance where the above function is called connects to the CRE node and finds out the CRE master's process id returning it as a tuple of the form `{ok, CrePid}`. The connection remains intact, so from the moment you received the CRE process id you can safely communicate with it.

#### Starting as an Erlang Application

You can start the CRE from an Erlang instance by calling

```erlang
cre:start().
```

Which is exactly the same as calling

```erlang
application:start( cre ).
```
This starts the CRE as an application under its own application master. This procedure also registers the CRE master process locally under the name `cre_master`.

#### Starting under the Default Supervisor

Alternatively, you can start the CRE's default supervisor by calling

```erlang
cre_sup:start_link().
```

This gives you the chance to embed the CRE supervisor in a custom supervision hierarchy. On success, the call returns a tuple of the form `{ok, CreSupPid}`. This procedure also registers the CRE master process locally under the name `cre_master`.

#### Starting Directly

You can also start the CRE master process directly. To start an unregistered instance of the CRE call

```erlang
cre_master:start_link().
```

On success, the call returns a tuple of the form `{ok, CrePid}`. Note that starting the CRE master process this way (unregistered) makes it impossible to locate it using `cre:pid/1`.

To start and register the CRE master process provide it with an appropriate process name. In the following example, we register the CRE locally, just as the default supervisor does:

```erlang
cre_master:start_link( {local, cre_master} ).
```

Like the argumentless variant of the `start_link` function the call returns a tuple of the form `{ok, CrePid}`. Note that starting the CRE master under a different name or using a different registry makes it impossible to locate the process using `cre:pid/1`.

### Controlling the Verbosity of Status Logs


### Creating a CRE Client Module

The CRE client is a service that takes a program from a user (or from another service) and computes its result. For that purpose, the client extracts from the program small, independent computational units, which we call applications, and sends them to the CRE master for evaluation. Conversely, the client expects the CRE master to reply with a result for each application it requested. The client continues to generate applications until all application results in a program are known and no new applications can be generated. Then the resulting program is returned to the user.

![cre Petri net model](priv/cre_client_pnet.png)

*Figure 2: Petri net model of the common runtime environment's client process. It provides a user interface (left) and an interface to the CRE master process (right).*

#### Starting a Client Process

Let's say we have created a CRE client in the module `logic_client`. The client must implement the `cre_client` behavior. We can start a client process by using the `cre_client:start_link/3` function. 

```erlang
cre:start().
{ok, CrePid} = cre:pid( node() ).
InitArg = [].
{ok, ClientPid} = cre_client:start_link( CrePid, logic_client, InitArg ).
```

We can query a client by using the `cre_client:eval/2` function. This function takes a CRE client pid and an expression `E` and returns the resulting value.

```erlang
E = {'and', {'not', true}, {'not', false}}.
cre_client:eval( Cre, E ).
```
The `logic_client` module and the expression `E` we used here exemplifies the zero-order logic we discuss in the [example section](#example-a-distributed-zero-order-logic). If the CRE has workers available, it starts scheduling. Consequently, the client request cannot complete, unless we add workers to the CRE master. How workers are implemented and added to the CRE master is described in the [worker module section](#creating-a-cre-worker-module).

The call to `cre_client:eval/2` is synchronous, i.e., the function blocks while the CRE application is busy and returns the plain value when it becomes available, in this case `false`.


#### Callback Functions

The CRE client is implemented by providing three callback functions:

- `init/1` is called when the client process is started using `cre_client:start_link/n`.
- `is_value/2` determines whether or not an expression is a value.
- `step/2` attempts to make progress on a given expression, returning a new expression and, if necessary, an application to be sent to the CRE master.
- `recv/4` reacts to the reception of a completed application.


##### init/1

```erlang
-callback init( InitArg :: _ ) -> UsrInfo :: _.
```
The `init/1` function is called when a client process starts. It takes an initial argument `InitArg`, and generates from it the user info field `UsrInfo` which is subsequently handed to the `is_value/2` and `step/2` functions. Herein, the initial argument is the same as the last argument handed to the `cre_client:start_link/n` function which is used to start up a client process.


##### is_value/2

```erlang
-callback is_value( E :: _, UsrInfo :: _ ) -> boolean().
```
The `is_value/2` function takes an expression `E` and the user info field generated by the `init/1` function and determines whether the expression is a value. If an expression is a value, that means that the program has terminated and the result is returned to the user.


##### step/2

```erlang
-callback step( E :: _, UsrInfo :: _ ) -> {ok, _} | {ok_send, _, _} | norule.
```
The `step/2` function takes an expression `E` and the user info field `UsrInfo` generated by the `init/1` function and generates either a new expression `E1` by returning `{ok, E1}` or a new expression `E1` and an application `A` by returning `{ok_send, E1, A}`. If no further progress can be made at the moment the function returns `norule`.


##### recv/4

```erlang
-callback recv( E :: _, A :: _, Delta :: _, UsrInfo :: _ ) -> _.
```

The `recv/4` function reacts to the reception of a application result. The function takes the current expression `E`, the application `A` that has been sent earlier and is now returned, the corresponding application result `Delta`, and the user info field `UsrInfo` as generated by the `init/1` function. It returns an updated expression `E1`.


### Creating a CRE Worker Module

The CRE worker consumes applications scheduled to it by the CRE master and evaluates them. For that purpose the worker also makes sure that any preconditions are met prior to evaluation and that any postconditions are met prior to sending the application's result back to the CRE master. Usually, a precondition is that all necessary input files are present. Conversely, a usual postcondition is that all expected output files have been created.

![cre Petri net model](priv/cre_worker_pnet.png)

*Figure 3: Petri net model of the common runtime environment's worker application. It provides an interface to the CRE master (left).*

#### Starting a Worker Process

As with the client process, the CRE master needs to run before we can connect workers to it. The worker module to be started is `logic_worker`. It implements the `cre_worker` behavior.

```erlang
cre:start().
{ok, CrePid} = cre:pid( node() ).
InitArg = [].
cre_worker:start_link( CrePid, logic_worker, InitArg ).
```

#### Callback Functions

The CRE worker is implemented by providing the following nine callback functions:

- `init/1` is called when starting a worker instance with `cre_worker:start_link/n`.
- `prepare_case/2` called upon receiving an application before any other application-related callback is used.
- `stagein_lst/2` returns a list of preconditions for a given application.
- `do_stagein/3` fulfills a precondition.
- `run/2` reduces an application assuming all preconditions are fulfilled.
- `stageout_lst/3` returns a list of postconditions for a given application and its evaluation result.
- `do_stageout/3` fulfills a postcondition.
- `error_to_expr/3` creates an error expression from a given intransient error.
- `cleanup_case/3` called upon finishing up a case prior to sending the result back to the CRE.

##### init/1

```erlang
-callback init( InitArg :: _ ) -> UsrInfo :: _.
```
The `init/1` function is called when the worker process starts. It takes an initial argument `InitArg` and generates from it the user info field `UsrInfo` which is subsequently handed to all other callback functions. Herein, the initial argument is the same as the last argument to the `cre_worker:start_link/n` function which is used to start a worker process.

##### prepare_case/1

```erlang
-callback prepare_case( A :: _, UsrInfo :: _ ) -> ok.
```

The `prepare_case/2` function is called every time an application is received and prior to any other processing steps. The function is intended for the worker to perform any preparation steps necessary to start processing the application `A`. The user info field `UsrInfo` has been generated using `init/1`. Upon success, the function returns the atom `ok`. Should anything in the preparation process go wrong, the function is supposed to raise an exception.

##### stagein_lst/2

```erlang
-callback stagein_lst( A :: _, UsrInfo :: _ ) -> [F :: _].
```
The stagein_lst/2 produces a list of preconditions `F` for a given application `A`. The user info field `UsrInfo` has been generated using `init/1`. Later, the `do_stagein/3` function will be called for each of the preconditions this function returns.

##### do_stagein/3

```erlang
-callback do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.
```
The `do_stagein/3` function fulfills a single precondition previously announced by the `stagein_lst/2` function. The function is expected to return the atom `ok` on success. In case of a *deterministic* error the tuple `{error, enoent}` should be returned, e.g., if an input file does not exist. In the case of a network outage or some other *transient* error, an exception should be raised.

##### run/2

```erlang
-callback run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.
```
The `run/2` function consumes an application `A` and attempts to reduce it. On success it is expected to return a pair `{ok, R}` containing the application's result `R` while in the case of a *deterministic* error it is expected to return a pair `{error, Reason}` containing the reason for the error. In case of a *transient* error an exception should be raised.

##### stageout_lst/3

```erlang
-callback stageout_lst( A :: _, R :: _, UsrInfo :: _ ) -> [F :: _].
```
The `stageout_lst/3` function takes an application `A` and its associated reduction result `R` and produces a list of postconditions `F`. Later, the `do_stageout/3` function will be called for each postcondition returned.

##### do_stageout/3

```erlang
-callback do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.
```
The `do_stageout/3` function fulfills a single postcondition previously announced by the `stageout_lst/3` function. The function is expected to return the atom `ok` on success. In case of a *deterministic* error the tuple `{error, enoent}` should be returned, e.g., if an output file has not been produced. In the case of a *transient* error, an exception should be raised.

##### error_to_expr/3

```erlang
-callback error_to_expr( A       :: _,
                         Reason  :: {stagein | stageout, [_]} | {run, _},
                         UsrInfo :: _ ) -> _.
```
The functions `do_stagein/3`, `run/2`, and `do_stageout/3` all carry the possibility to produce a deterministic error. This possibility is usually reflected in the target language by providing a syntactic category for errors and reduction rules that handle errors. The `error_to_expr/3` function takes an application `A` and an error info field `Reason` and produces from these an error expression in the syntax of the target language.

##### cleanup_case/3

```erlang
-callback cleanup_case( A :: _, R :: _, UsrInfo :: _ ) -> R1 :: _.
```

The function `cleanup_case/3` is called whenever an application has been fully processed and the result is ready to be sent back to the CRE master. The arguments are the application `A`, its result `R`, as well as the `UsrInfo` field as generated by `init/1`. The function returns an updated result expression `R1`. Should cleaning up fail, the function is expected to raise an exception.


## Example: A Distributed Zero-Order Logic

In this section we demonstrate the implementation of a CRE-based programming language by creating a distributed zero-order logic, i.e., a logic with truth values and propositional operators like negation, conjunction or disjunction but no quantifiers or variables. We show how a client module and a worker module are implemented from the callback definitions we gave in the previous section.

There are several reasons why distributing a zero-order logic this way is problematic. However, the example is instructive because there is a habitual familiarity of programmers with logic and also because it is a healthy exercise to reflect on when *not* to distribute.


### Reduction Semantics

First, let us clarify what we are about to build. Here, we define a zero-order logic as a reduction semantics, i.e., we give a notion of reduction which we apply in an evaluation context to get a small-step operational semantics.

So, first, we define the syntax of the language. Then, we give the notion of reduction and a definition of the evaluation context. The interesting part is the small step reduction relation that we create from these building blocks. This reduction relation as well as the syntax of programs needs to conform some basic rules in order to be compatible with the CRE.

#### Syntax

Here, we introduce the static syntax of the zero-order logic. It consists of truth values, negation, conjunction, and disjunction. The resulting syntax for expressions *e* looks as follows:

![Syntax: expression first version](priv/logic_syntax_expr1.png)

#### Notion of Reduction

Before we introduce the notion of reduction for the above syntax, we need to extend the syntax by defining the concept of a value *v*, i.e., an expression that can be the result of an evaluation and that can play the role of an operand in a reducible expression:

![Syntax: value](priv/logic_syntax_value.png)

Now we are ready to define the notion of reduction in form of the binary relation n. There should be no surprises here.

![E-neg-true](priv/logic_e-neg-true.png)

![E-neg-false](priv/logic_e-neg-false.png)

![E-and-true](priv/logic_e-and-true.png)

![E-and-false](priv/logic_e-and-false.png)

![E-or-true](priv/logic_e-or-true.png)

![E-or-false](priv/logic_e-or-false.png)

#### Evaluation Context

Before we introduce the reduction relation for the distributed zero-order logic we need to define the syntax of evaluation contexts:

![Syntax: evaluation context](priv/logic_syntax_evaluation_context.png)

Note that defining the evaluation context this way results in a non-deterministic reduction relation. This non-determinism allows us to find reducible expressions in many places inside an expression. This is important when we define the reduction relation for the CRE, since we want to send as many reducible expressions as possible to the distributed execution environment regardless of how fast the CRE can generate replies.

#### Reduction Relation: A First Try

To get a reduction relation from the previously defined notion of reduction n, we create the compatible closure of n by applying it in the evaluation context *E*:

![E-red](priv/logic_e-red.png)

`[E-red]`

#### Reduction Relation: The CRE Way

The reduction relation defined in `[E-red]` describes how evaluation in our zero-order logic can be accomplished. However, it applies the notion of reduction in-place, thereby reducing one redex at a time in a sequential manner. However, our goal is to send the redex to a remote service instead of just reducing it and to do so with as many redexes as we can find in an expression.

Accordingly, the first thing to do is to extend the syntax of expressions *e* with the concept of a future. Futures provide a way to mark redexes that have already been sent away to be computed.

![Syntax: expression second version](priv/logic_syntax_expr2.png)

Next, it is not enough to operate bare expressions. We need a queue of redexes for which we request evaluation. Also, we need a way a queue of redex-value pairs we received in return.

Accordingly, a CRE program *p* is a triple consisting of a send-queue, a receive-queue, and a control string expression. The send-queue is a list of redexes awaiting reduction, the receive-queue is a list of redex-value pairs holding the redex and its value derived by applying the notion of reduction, and the control string is the expression under evaluation.

![Syntax: program](priv/logic_syntax_program.png)

Now, we need to define a reduction relation on programs instead of expressions. The updated reduction relation consists of two rules. The first rule defines how redexes are sent to the execution environment. This is achieved by enclosing a redex in a future and by adding the redex to the send-queue.

![E-send](priv/logic_e-send.png)

`[E-send]`

Next we need to define how results which have been received are substituted into the control string:

![E-recv](priv/logic_e-recv.png)

`[E-recv]`

The notion of reduction n does not appear directly in the reduction relation anymore (we use it only in a side condition to identify redexes in `E-send`). This reflects the fact that the notion of reduction is applied by the worker and, thus, never explicitly appears in the way reduction is performed in the client.

In real programming languages, we also need the client perform some reductions and try to defer only the expensive tasks to the CRE. In this example, however, we have the CRE do *all* reductions.

### A CRE Application from the Reduction Semantics

#### Syntax

The type specifications we make here are rather for documentation than for anything else. We give them anyway because they give us a feeling for what the abstract syntax we defined for the reduction semantics has to do with Erlang terms.

The syntax of expressions is made up of Booleans and tuples.
```erlang
-type e() :: boolean()
           | {'not', e()}
           | {'and', e(), e()}
           | {'or', e(), e()}
           | {fut, e()}.
```
Similar to the syntax of expressions we can give a definition of the syntax of evaluation contexts.
```erlang
-type ctx() :: hole
             | {'not', ctx()}
             | {'and', ctx(), e()}
             | {'and', e(), ctx()}
             | {'or', ctx(), e()}
             | {'or', e(), ctx()}.
```

#### Implementation of the Worker

The CRE worker for our zero-order logic involves implementing the notion of reduction inside the `run/2` function. This function takes a redex and returns the result of that redex. The user info field is ignored.

```erlang
run( {'not', X}, _UsrInfo )      -> {ok, not X};
run( {'and', X1, X2}, _UsrInfo ) -> {ok, X1 andalso X2};
run( {'or', X1, X2}, _UsrInfo )  -> {ok, X1 orelse X2}.
```

The worker also requires implementing eight other callback functions not shown here. The [source code](test/logic_worker.erl) for the whole worker module is available as part of the CRE test suite.


#### Implementation of the Client

The CRE client for our zero-order logic involves implementing the two reduction rules `[E-send]` and `[E-recv]` from the reduction semantics. Additionally, we have to implement a test whether evaluation has terminated. Concretely, we need to implement the three callback functions `init/1`, `is_value/2`, and `step/2`. The [source code](test/logic_client.erl) for the whole client module is available as part of the CRE test suite.

##### init/1

The `init/1` function needs to generate the user info field which is later handed to all other callbacks. Since we do not make use of the user info field here we just ignore the initial argument and return a dummy term `[]`.

```erlang
init( _InitArg ) -> [].
```

##### is_value/2

The `is_value/2` function tests whether an expression is a value or not telling the CRE client when evaluation has terminated. In the case of our zero-order logic, an expression is a value when it is a plain truth value, i.e., `true` or `false`.

```erlang
is_value( E, _UsrInfo ) -> is_boolean( E ).
```

##### step/2

The `step/2` function implements a small-step semantics for the language to be interpreted. In the case of our zero-order logic the function implements the `[E-send]` rule of the reduction relation.

```erlang
step( E, _UsrInfo ) ->
  case find_context( E ) of
    {ok, {Ctx, Redex}} -> {ok_send, in_hole( Ctx, {fut, Redex} ), Redex};
    {error, nocontext} -> norule
  end.
```


##### recv/4

The function `recv/4` reacts to the reception of an application result. In the case of our zero-order logic the function implements the `[E-recv]` rule of the reduction relation.

```erlang
recv( E, A, Delta, _UsrInfo ) ->
  subst_fut( E, A, Delta ).
```

The functions `step/2` and `recv/4` are defined in terms of three other functions: `find_context/1`, `in_hole/2`, and `subst_fut/3`. `find_context/1` takes an arbitrary expression and tries to decompose it into an evaluation context and a redex. `in_hole/2` replaces the hole in an evaluation context with some expression (or another evaluation context). `subst_fut/3` replaces a future with its corresponding value.

Note that even though the reduction rules we gave here are encoded as deterministic functions, evaluation is not deterministic w.r.t. the order of reductions. The reason is (i) that we do not stop sending redexes after we identified the first one but traverse each "leaf" of the syntax tree until all redexes have been sent, and (ii) that reduction results can be received in any order even though the client has sent the redexes away in a deterministic order.

## Related Projects

- [Hadoop YARN](https://hadoop.apache.org/docs/current/hadoop-yarn/hadoop-yarn-site/YARN.html) is a large-scale data analysis platform that performs resource- and application management. YARN's resource manager is comparable with the CRE since it manages data analysis workloads by performing scheduling and failure recovery for a number of containers.
- [HTCondor](https://research.cs.wisc.edu/htcondor/) is a high-throughput data analysis platform. It is the underlying execution environment for [Pegasus](https://pegasus.isi.edu) whose workflow specification language is Pegasus DAX.
- [Toil](https://github.com/BD2KGenomics/toil) is a scalable pipeline management system. It is an execution environment running workflows specified in [Common Workflow Language (CWL)](https://github.com/common-workflow-language/common-workflow-language).
- [GenStage](https://github.com/elixir-lang/gen_stage) manages communication in producer-consumer networks. Thus, similar to the CRE, it is an application-independent scaffold which can be specialized to data analysis workloads.

## System Requirements

- [Erlang](https://www.erlang.org) OTP 19.0 or higher
- [Rebar3](https://www.rebar3.org) 3.0.0 or higher

## Resources

- [joergen7/gen_pnet](https://github.com/joergen7/gen_pnet). A generic Petri net OTP behavior, the CRE is based on.
- [joergen7/cuneiform](https://github.com/joergen7/cuneiform). A functional language for large-scale data analysis whose distributed execution environment is implemented on top of the CRE.
- [joergen7/cf_client](https://github.com/joergen7/cf_client) CRE client implementation for the Cuneiform distributed programming language.
- [joergen7/cf_worker](https://github.com/joergen7/cf_worker) CRE worker implementation for the Cuneiform distributed programming language.

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)