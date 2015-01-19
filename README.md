# Appupper - Automatic .appup generation

It's intended to work alongside ````relx````, and reads the ````relx.config````

## Build and install

Create a standalone executable escript and copy into your PATH:

    $ make
    $ sudo cp ./appupper /usr/bin/

## Raison d'être

To automatically bump versions of apps and the release, and create
suitable appup files. So you can just change one .erl file, run this
tool, and be ready to ````relx release relup```` without further ado.

Should probably enforce that there are no uncommitted mods, so we get
one clean commit with our appup related changes.

## Usage

You have a _rel/myrelease directory, and have built your first release.
ie, the _rel/myrelease/releases/1 directory exists.

You edit an .erl file and recompile. Time to bump the version in the
.app/.app.src file for the application that module belongs to, and also
to bump the release version, from the relx.config.

We'll also need an appropriate .appup file to load the changed module in
the new version of the application.




