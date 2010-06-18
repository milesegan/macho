--------------------------------------------------------------------------------
                           INSTALLING MACHO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
STEP 1 - OBTAIN SBCL
--------------------------------------------------------------------------------
Macho now comes complete with all libraries required for operation(*).  Its only
external dependency is SBCL itself.  Macho is tested with versions of SBCL
greater than 0.8 only.

SBCL is available from http://sbcl.sf.net.  Easy to install rpms are available
for Redhat Linux.  Debian Linux provides SBCL as a standard package.  Binary
packages may be available for other platforms.

--------------------------------------------------------------------------------
STEP 2 - INSTALLING THE MACHO DISTRIBUTION
--------------------------------------------------------------------------------
Unpack the Macho distribution anywhere you find convenient.  It will run from
any location.

Next, run "make" to compile macho.  This might take a minute or two.

Note: you will probably have to rebuild macho by running "make clean; make" if
you upgrade SBCL to a newer version at some point after building macho.

--------------------------------------------------------------------------------
STEP 3 - SETTING UP ARCHIVES
--------------------------------------------------------------------------------
a. For each mail archive for which you'd like Macho to maintain web archives,
   create a subdirectory under the "root/archives" directory.  Macho will interpret
   the name of this directory as the name of the list for display purposes in the
   generated archives.  For example, to build an archive for the "clump" mailing
   list, create a the directory roots/archives/clump.

b. Currently, Macho will only archive messages from an mbox format file. Copy or
   symlink the mbox file for the archive under the directory you created in step
   a.  For instance, the the clump lists mbox file would be copied to
   root/archives/clump/mbox.  If the archive is active, it's up to you to arrange
   for new messages to be delivered to this file.  Macho will automatically
   recognize and archive any new messages delivered to the mbox file, but it
   doesn't perform any mail delivery functions itself.

--------------------------------------------------------------------------------
GENERATE ARCHIVES
--------------------------------------------------------------------------------
Run the top-level "macho update" command in the Macho directory with the name of
your list or lists as its arguments.  For instance, to generate archives of the
clump list, run the following command:

./macho update clump

The macho command accepts a few options.  Run it with no arguments to see what
options it offers.

Macho will generate archives in its "root" directory, under
root/archives/archivename.  The clump archive, for instance, will be archived in
root/archives/clump.

--------------------------------------------------------------------------------
CUSTOMIZE ARCHIVES
--------------------------------------------------------------------------------
Macho permits per-archive style customizations.  To customize the look of your
archive just add a css stylesheet named "custom.css" in your 
root/archives/archivename.  Have a look at the main stylesheet in 
root/macho.css to get a list of styleable macho objects.

--------------------------------------------------------------------------------
PUBLISH ARCHIVES
--------------------------------------------------------------------------------
Publishing Macho archives is simply a matter of placing them somewhere in the
document root of your webserver.  The easiest way to do this is to symlink
Macho's root directory somewhere in your servers document directory.

Installation is complete at this point.  If your list is active and still
receives mail, you have can keep your archives up to date in one of two ways:

1. run "macho update" out of cron
This is probably the simplest method.  It has the disadvantage that macho will
run at regular intervals whether there is new mail or not and that there is a
delay between the time a message arrives and the time it appears in the
archives.

2. set up an email alias to deliver to the "macho deliver" comand
This is somewhat more difficult to set up but has the advantage that
macho runs only when necessary and that new messages will appear in
the web archives right away.  Setting this up properly involves some
understanding of unix mail delivery agents, but typically it involves
adding an alias to the /etc/alias file that looks something like this:

my-alias: "|/path/to/macho deliver my-alias"

where "my-alias" is the name of your archive, and then running the
"newaliases" command.

*Note*: this will only work if your archive directories are writable
by the user as which the mail client runs on your system.


*** IMPORTANT NOTE ***
Although Macho bundles all of its dependent
libraries, there is no guarantee the libraries distributed with Macho
are the most up-to-date.  Although it's recommended that you use the
versions distributed with macho, you should obtain the most recent
versions of these libraries if you plan to use them yourself in
another project.

The most recent versions of Edi Weitz's cl-ppcre and html-template
libraries can be found at [his site](http://www.weitz.de).

Dan Barlow's handy net-telent-date library can be found
at [cliki](http://www.cliki.net/net-telent-date)
