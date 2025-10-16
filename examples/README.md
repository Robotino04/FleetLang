# Examples

There are two kind of examples for fleet: tested and untested examples.
Tested examples are ones that get compiled and ran for every commit while untested ones are not guaranteed to work.
Untested examples are however more complex and demonstrate more of the language so they are more interesting.
You can find the untested ones in this directory and the tested ones are located in `/fleet/src/tests/examples`.
`/fleet/src/tests/{valid,format,invalid}` also contain lots of other tests that get ran every commit.
If you are looking for the expected behavior of a certain structure, that's the place to look at.

> [!NOTE]
> Because Fleet doesn't currently support heap-allocating objects, most examples use a LOT of stack space.
> So these probably won't run without making the stack bigger.
> On Linux that can be done using this command:
> ```sh
> ulimit -s 650000 
> ```
> Keep in mind that you'll need to do this in every terminal that you want to run Fleet binaries in.
