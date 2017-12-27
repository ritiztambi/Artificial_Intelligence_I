;[12/7/2017 4:52 PM] Ritiz Tambi

;Define the Goal State here : 
(defparameter *sGoal* '(1 2 3 4 5 6 7 8 - ))






;Every Node has four parameters. State, Parent Node, G-Value, F-Value
;The following functions are created to access these values of any node.
(defun State (Node) (First Node))

(defun Parent (Node) (Second Node))

(defun G-Val (Node) (Third Node))

(defun F-Val (Node) (Fourth Node))






;This function gives the heuristic value from the current state using displaced tiles method.
;Number of in-place tiles and are computed and then deducted from total number of tiles (9)
;to give heuristic value.
;Output - heuristic value
(defun H-Val (curr_state)
  (if(eq curr_state NIL) (return-from H-Val 99999))
  (setf sGoal *sGoal* )
  (setf tiles 0)
  (dotimes(counter 9)
    (if(eq (nth counter curr_state) (nth counter sGoal)) (setf tiles (+ tiles 1)))
    collect tiles)
  (return-from H-Val (- 9 tiles)))






;This function gives the index of any element in the node puzzle-state.
;This function is used by functions moving the Space.
;Output - index value
(defun index (el l pos)
  (if (null l) (error "Can't find ~s in function INDEX~%" el))
  (if (eq el (car l))
      pos
    (index el (cdr l) (incf pos))))






;This function swaps two values given by their indexes p1,p2 in the given node puzzle-state
;Output - Puzzle State with swapped values.
(defun swap (state p1 p2 &aux temp state2)
  (setf state2 (copy-tree state))
  (decf p1) 
  (decf p2)
  (setf temp (nth p1 state2))
  (setf (nth p1 state2) (nth p2 state2))
  (setf (nth p2 state2) temp)
  (return-from swap state2))







;This function moves the Space right.
;Output - new Puzzle State 
(defun move-space-right (state &aux pos)
  (setf pos (index '- state 1))
  (if (not (or (eq pos 9) (eq pos 6) (eq pos 3)))
      (swap state pos (+ pos 1))))
  
;This function moves the Space up.
;Output - new Puzzle State
(defun move-space-up (state &aux pos)
  (setf pos (index '- state 1))
  (if (> pos 3)
      (swap state pos (- pos 3))))

;This function moves the Space down.
;Output - new Puzzle State
(defun move-space-down (state &aux pos)
  (setf pos (index '- state 1))
  (if (< pos 7)
      (swap state pos (+ pos 3))))
  
;This function moves the Space left.
;Output - new Puzzle State
(defun move-space-left (state &aux pos)
  (setf pos (index '- state 1))
  (if (not (or (eq pos 7) (eq pos 4) (eq pos 1)))
      (swap state pos (- pos 1))))








;This function generates all the children nodes possible for a given node by moving the
;space right, up, left, down. Path cost and F-values of children nodes and also computed
;and added to the nodes.
;Output - Children Node List
(defun children-generator(node)
    (setf child-node-l nil)
    (setf sCurrent (State node))
    (setf states ())
    (setf sNew1 (move-space-right sCurrent))
    (if (not (null sNew1)) (push (list sNew1 node (+ (G-Val node) 1) (+ (+ (G-Val node) 1) (H-Val sNew1))) child-node-l))
    (setf sNew2 (move-space-left sCurrent))
    (if (not (null sNew2)) (push (list sNew2 node (+ (G-Val node) 1) (+ (+ (G-Val node) 1) (H-Val sNew2))) child-node-l))
    (setf sNew3 (move-space-up sCurrent))
    (if (not (null sNew3)) (push (list sNew3 node (+ (G-Val node) 1) (+ (+ (G-Val node) 1) (H-Val sNew3))) child-node-l))
    (setf sNew4 (move-space-down sCurrent))
    (if (not (null sNew4)) (push (list sNew4 node (+ (G-Val node) 1) (+ (+ (G-Val node) 1) (H-Val sNew4))) child-node-l)))








;The following functions update the closed list in A* loop by removing the 
;children-nodes whose states have already been visited in the Tree.
;Output - Updated Closed List.
(defun remove-bad-children(children-nodes closedList)
  (setf new-list())
  (loop for node-child in children-nodes do
      (if (not (remove-nodes-in-closed node-child closedList))(push node-child new-list)))
  (remove nil new-list)
  (return-from remove-bad-children new-list))

(defun remove-nodes-in-closed(child-node list-closed)
  (loop for n in list-closed do
  (if(equalp (State n)(State child-node))(return-from remove-nodes-in-closed T) ))
  (return-from remove-nodes-in-closed nil))










;This function creates a list consisting of pairs such as ((1 2)(1 3)(1 4)....(2 5)(2 6)....)
;This function is used by is-feasible function to detect feasible states.
;Output - Pair List
(defun create-pair-list (start_state)
  (setf list1 nil)
  (loop for outer from 0 to 6 do
    (loop for inner from (+ 1 outer) to 7 do
      (if(and (not (eq nil (nth inner start_state))) (not (eq nil (nth outer start_state)) )) (push (list (nth outer start_state) (nth inner start_state)) list1))))
  (return-from create-pair-list list1))


(defun remove-space (Node)
  (setf l (remove '- Node)))



;This function detects feasible states by checking whether the number of inversions are even or odd.
;Two Pair lists are generated corresponding to start-state and goal-state, which are then 
;compared to count the number of inversions.
;Output - True or Nil
(defun is-feasible (start-state goal-state)
(setf list-start(remove-space start-state))
(setf list-goal(remove-space goal-state))
(setf listS (create-pair-list list-start))
(setf listG (create-pair-list list-goal))
(setf common 0)
  (loop for x in listS do
    (loop for y in listG do
      (progn
        (if(and (eq (first x) (first y)) (eq (second x)(second y)) ) (setf common (+ 1 common))))))
  (setf inversion_count 0)  
  (setf inversion_count (- (list-length list1) common))
  (if(eq (mod inversion_count 2) 0)(return-from is-feasible T)(return-from is-feasible NIL)))










;This function traces back the path from Goal-State to trace the solution branch of the A* SearchSpace Tree.
;Output - Solution List
(defun Solution (Node)
  (setf solution-list ())
  (loop
      (push (State Node) solution-list)
      (push '|| solution-list)
      (if (null (Parent Node)) (return solution-list))
      (setf Node (Parent Node))))









;This function performs A* Search on the start node until the goal-state is reached.
;Two lists Open and Closed are maintained and updated in the loop corresponding to
;the states to-be-visited and states already-visited respectively. Open List is sorted 
;to provide the functionality of a priority queue for F-Values.
(defun A-star (sStart)
 (let (( open (list (list sStart nil 0 (H-Val sStart)))) 
        (closed nil)
        (sNew nil)
        (children nil)) 
        (setf counter 0)
        (if (not (is-feasible sStart *sGoal*)) (return-from A-star 'Infeasible_State))
        (loop
          (incf counter)
          (print counter)
          (if (null open) (return 'failure)) 
          (setf sNew (pop open))  
          (format t "SCurrent: ~a ~%" (State sNew))

          (push sNew closed)  
          (if (equalp (state sNew) *sGoal*) (return (Solution sNew)))    
          (setf children (children-generator sNew)) 
          (setf open  
             (sort open 
                   #'(lambda(node1 node2) (< (F-Val node1) (F-Val node2)))))
          (setf children (remove-bad-children children closed))         
          (setf open (append open children))
          (setf open
              (sort open
                    #'(lambda(node1 node2)(<(F-Val node1)(F-Val node2)))))))) 





  