###CAM:  A Common Lisp library for Content Addressable Memory.

Think of a _cam_ object as an associative array with variable dimensionality.
For example:

		cam["foo"] = 0
		cam["file_name.c"][1955] += 1
		cam["file_name.c"][2017] += 1
		cam["one hand"]["other hand"]["gripping hand"] = "Crazy Eddie"

Internally, _cam_ objects are represented as a hash table of hash tables.

[Function]<br>
**camref** (_cam_ &rest indices)

		Retrieve the value of cam[index1][index2]...  The cam's default value is returned if there is
		no value at the supplied indicies.
		
(setf (**camref** &indices) value)

		Set the value of cam[index1][index2]...

[Function]<br>
**make-cam** (&key (default-value nil) (default-test #'equal) (specialized-tests nil))

		Create a cam object, which is a hash table of hash tables.  default-value will be returned if
		a camref accesses an item that has not been placed into the cam.  default-test specifies
		the :test function to use with make-hash-table whenever the cam has to create an additional
		hash table and no specialized test is available.  specialized-tests is a list of :test functions
		to use when creating hash tables.  The first item is used for the first level hash table, the
		second for second level hash tables, and so on.  A test function must be one of #'eq, #'eql,
		#'equal, or #'equalp.
    
		For example, to profile file and line number pairs, you might:
    
			(defparameter *profile* (make-cam :default-value 0 :specialized-tests (list #'equalp #'eql)))
			(incf (camref profile "thisfile.c" 127))
			(incf (camref profile "thatfile.md" 36))

[Function]<br>
**walk-cam** (cam predicate)

	Walks the _cam_ object and calls _predicate_ with a list of indices and the corresponding value.
	That is, _predicate_ is #'(lambda (incides value) ...)

####License
CAM is available under a BSD-like license.  See the file LICENSE for
details.

#### Contact
For any questions or comments, please feel free to email me, Bob Felts
<wrf3@stablecross.com>
