This Shiny application demonstrates a basic workflow for application 
development. These steps include:

1. Definition of application scope
    - What is the desired base functionality?
    - What are the use cases for this application?
    - What is actually needed to meet these requirements?
2. Generation of an R script that achieves the scope in a scripted 
(not interactive) fashion by separating:
    - Values that the user will interact with (inputs)
    - Components that depend on inputs (reactive objects)
    - Components that don't depend on inputs (non-reactive objects)
    - Outputs that are generated as a result of simulation (reactive outputs).
3. Setting up the global environment by adding all non-reactive code to `global.R`.
    - As easy as copy and paste
3. Creation and prototyping of a user-interface with the desired layout in `ui.R`. 
    - It is important to involve your users with this part
    - Create something they want to use
    - `server.R` can be empty while you do this, as long as you don't include
    any output elements your `ui.R` (functions with `Output` as a suffix) 
4. Setting up interaction between user-interface and simulation code (`ui.R` and 
`server.R`).
    - Making changes in pairs when adding new inputs, each input widget added 
    to `ui.R` should have a corresponding change in `server.R`.
    - Making changes in pairs when adding new outputs, each rendered output in 
    `server.R` should have a corresponding output element in `ui.R`.

The last part is only partly done in this example, as building a Shiny app is
an iterative process. Adding one piece of functionality at a time makes it
easier to understand what is causing errors when they inevitably show their face.

**THIS IS AN EXAMPLE APPLICATION AND IS FOR DEMONSTRATION PURPOSES ONLY.**