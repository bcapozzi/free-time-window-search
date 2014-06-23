# free-time-window-search
Implementation of a search algorithm based on finding the earliest time that a given user can reach the "goal" from a given "start" through a set of capacity-constrained "resources", given a loading function on those resources.  

Basic idea is to, given a loading function of each resource, first identify the "free" time-windows on each resource.  Then, starting with the initial resource, perform an A*-like search to "expand" to the next reachable free-time-window on any downstream neighbor resources.  Once the "goal" resource is reached, the path (resource, entry time) from start to goal can be re-constructed using the back pointers that are assigned as the search progresses. 
 
Based on the paper:  
ter Mors, A.W., Zutt, J., and Witteveen, C., "Context-Aware Logistic Routing and Scheduling", ICAPS2007


