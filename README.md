# SPIRE Event Server

[SPIRE = Smart Parking for Intelligent Real-Estate](https://www.hiit.fi/spire), a research project
running between 2013-2014. This repository contains two components:

1) Server. Keeps data in an RDF graph using a Sesame
server. Communicates with a mobile client (repository to be added in
the near future) and infrastructure sensors. Contains functions for:
* Client profile check and registration
* View update including parking lot statuses and other destinations
* Destination setting
* Parking area info update (push)
* Sensor input processing (both mobile clients and infra sensors)
* End-user survey push

2) Sensor polling loop. A specialized component for interfaces
established between two infrastructure sensor providers, to
periodically poll for new parking lot entry and exit events. Also
contains code to download a picture of the parking area, if one is
available.

[The corresponding client](https://github.com/aaltodsg/spire-client) is available in github.

# Status of work

The project has finished and the code in the repository is made
available as-is. No development plans, no support and absolutely no
guarantee of the code being suitable for any purpose.

Please note especially that the server does not implement any
security. Furthermore no parameter checking of REST calls is done, so
the current code is completely open to any SPARQL (Update!) injection attacks imaginable.

# Server Installation

[Hint: In OS X most of these are available via [homebrew](http://brew.sh/). In Unix environments a package manager such as aptitude will do the same.]

1) Configure addresses and keys as needed. A list with file names and line numbers can be obtained by running (in the "src" directory):

    $ grep -nH 'INIT_CONFIG' *.lisp

2) Install a Java servlet. Our system has been running on [jetty](http://eclipse.org/jetty/)-8.1.11.v20130520.

3) Get [Sesame](http://rdf4j.org/). From the Sesame package, copy "openrdf-sesame.war" and "openrdf-workbench.war" to directory "libexec/webapps" under Jetty. Test run Jetty by running (in the Jetty libexec directory):

    $ java -jar start.jar

If everything is ok, using a browser to open "http://localhost:8080/openrdf-workbench/" (in the same machine, otherwise substitute "localhost" with the server address) should bring up the Sesame Workbench.

[Hint: Jetty stops by pressing Ctrl-C.]

4) Create a new repository called "spire" in Sesame. Select the type
of the repository as needed, a "Native Java Store" will do fine for testing.

5) To populate the repository with some user data, edit the sample
SPARQL update file `init/spies_initialize.ru` as needed. Select
"SPARQL Update" in the Sesame workbench and copy the file contents
into the Update text box. Execute.

6) Install [SBCL](http://www.sbcl.org/).

7) Install [Quicklisp](https://www.quicklisp.org/beta/).

8) [ASDF](https://common-lisp.net/project/asdf/) is also needed. Nowadays it is bundled with SBCL, but at least
lately on Linux systems it has required activation. Run sbcl:

    $ sbcl

Load the module with:

    * (require "asdf")

This line should return: ("ASDF"). Make sure it is the latest version by using asdf to load itself:

    * (asdf:load-system :asdf)

Should return: T. SBCL can now be exited with (quit)

9) Confirm that the home folder of the project is configured. Under
the current home directory, find (or make) directory:

    ~/.config/common-lisp/source-registry.conf.d

In source-registry.conf.d create file: "spire.conf" with the path to the cloned "src" directory:

    (:directory "~/xx/yy/spire-server/src/")

10) Run SBCL and type:

    * (asdf:load-system :spies)
[ A lot of style warnings follow. ]

    * (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)) 

This should return the following object:

    #<HUNCHENTOOT:EASY-ACCEPTOR (host *, port 4242)>

At this point "spire.address.com:4242" should respond to a browser and the server should be happily up and running.

If loading :spies complains about missing drakma / hunchentoot / cl-json, they can be loaded like this:
    * (asdf:load-system :drakma)
    * (asdf:load-system :hunchentoot)
    * (asdf:load-system :cl-json)

# Documentation

Further information is available in the "SPIRE_EventServer.rtf"
document in the repository root.

A
[conference publication](http://www.cs.hut.fi/~mjrinne/papers/its-europe2014/Mobile%20crowdsensing%20of%20parking%20space%20using%20geofencing%20and%20activity%20recognition%20-%20ITSEur2014.pdf)
and an accompanying
[presentation](http://www.cs.hut.fi/~mjrinne/papers/its-europe2014/Presentation%20-%20Mobile%20crowdsensing%20of%20parking%20space%20using%20geofencing%20and%20activity%20recognition%20-%20ITSEur2014.pdf)
are also available.
