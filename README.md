# Hatchling 32

DISCLAIMER: The code in this project is severely embryonic in nature and will only 
work in particular ways under certain conditions. The idea is to be plummeting down a single 
vertical as far as possible and bother little with details.

## Purpose of this project

The purpose of this project was to learn. One thing I wanted to learn about was programming in Scala, and by taking on a task which I knew required me to dive into a lot of detail motivated by a goal of getting things to work little by little. This required me to get a whole lot of stuff completely right, and allowed experimenting with dense functional definitions for a lot of the repetitive details in the instruction set.

## What the code does

The Scala code in this repository implements an emulator for the Motorola M6809 CPU configured with I/O and memory corresponding to the legendary Dragon 32 microcomputer. The Dragon was sold roughly throughout the years 1981-1985 and was marketed towards consumers along with a selection of games and office-related tools. It sold far less than its contemporary competitor the Commodore 64, and was quickly abandoned and forgotten by the general public. 

Its architecture was primitive but efficient, and quite suitable for learning about computers and how they work in general. This stems partly from the design choice of using a relatively powerful CPU, compensating for a lack of task-specific hardware like the SID-chip for generating sound in the C64.

The Dragon had a variant of Microsoft Basic pre-loaded from ROM, which could also be leveraged by non-basic software by calling into vector tables pointing to useful subroutines rather than bundling everything.

With only ta few exceptions this emulator implements all of the M6809 instruction set, including undocumented features specific to the Dragon 32. Undocumented features were common to include and use, especially in copy-protection schemes, where they attempted to confuse anyone trying to reverse-engineer them. 

"Donkey King" was the name of a common game on the platform, and was subject to a trial forcing it to later change its name to "The King". For this project it serves as a spike implementation, presenting a number of things to support, like color graphics, polyphonic music, sound effects, joystick/keyboard input, loading from cassette and copy protection via undocumented features. The emulator allows the first game screen to start, objects start moving, but your protagonist Mario quickly turns and walks into the burning barrel at the bottom, ending his life. Rip.
