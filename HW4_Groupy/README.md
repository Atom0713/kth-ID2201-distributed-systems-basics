# Groupy - a group membership service
The purpose of this assignment is to create a group off processes with coordinated state. 
Group behaviour should be synchronized using atomic multicast approach. Each state change has to be communicated 
to all group memebers before it is executed. Keeping processes synchronized is the main challenge sicne processes
can crash and be replaced by a new group member. The role of multicasting is assumed by the group leader and all 
state change requests must go through the group leader. An external node can requests to become a memeber through 
any group member, however group leader will decide when to include new group member. 

## View synchrony
Each member node should be able to multicast message to the rest og the group.
For all messages we guarantee the following:

- FIFO
- in total order: all nodes see the same sequence
- reliability

Message delivery is not guaranteed. Messages are sent in an asynchronous manner without acknowledgment.

## The leader
Node is either a leader or a subordinate. Each subordinate will send message to the leader. Leader will mark that message with the sequence number and multicast it to the group. Leader can receive messages from it's owner, application-layer. The application-layer if it's group process is aleader or a subordinate.

## The subordinate
Subordinate will receive messages from application-layer and forward them to the leader, unless it's the leader. It will also receive messages from the leader and forward them to the application-layer.

## Leader election
All subordinates have the same list of peers and elect first node as new leader. New leader must resend last message received and subordinates must monitor the leader.

## The application layer
Application process can request to enter the group but it can't monitor the group in anticipation of the acceptance. Therefore, application process can timeout the request if it was not invited. Once accepted application process needs to request the current state and wait to receive message from the multicast layer. Notification of the current state might not be the first message received by the new member, depnding on the pipeline. Once requested state is received, state changes must be applied in order before process is up and running.