## Day 15

### Part 1

The approach taken here is, in essence, Djikstra's shortest path algorithm where each two neighbors are assumed to have directional edges between them. Specifically, the weight of an edge is equal to the "risk" of the node which it enters. 