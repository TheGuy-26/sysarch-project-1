# Contributions

In the beginning, we basically distributed the tasks equally between the two of us as follows. However, by the end of the project, we were testing and debugging each other's tasks together, thereby, getting an overall understanding of the entire project.

### Achyut Raj
Tasks: 1.1, 1.2, 2.2, 2.5

### Dujana Abrar
Tasks: 1.3, 1.4, 2.3, 2.4

# Use of AI tools

We used ChatGPT for high level questions about the RISC-V specification, Chisel syntax and for some help with debugging.

## Problem 2.6: Bonus

We used a combination of two approaches:
- Bruteforce based approach only for single-instruction permutations, in order to not generate 30 instructions for something like a simple rotation.
- Swap-based iterative algorithm that generates a series of the basic operations for one pair of bits at a time, until eventually obtaining the required permutation.
