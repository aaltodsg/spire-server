(in-package :spies-asd)

;;; SPIRE Event Service

;; Server Implementation

;; mikko.rinne@aalto.fi 2014
;; License MIT: http://opensource.org/licenses/MIT

;; Modified by Mikko Rinne 11.5.2015 replaced addresses and passes with placeholders and INIT_CONFIG comment tags.

;; Helper functions:

; Return current formatted date-time
(defun datetime-now ()
  (multiple-value-bind
	(second minute hour date month year)
      (get-decoded-time)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
	    year
	    month
	    date
	    hour
	    minute
	    second))
  )

; Request double floats - GPS coordinates don't work without it:
(setq *read-default-float-format* 'double-float)

; Return first number from a comma-separated string
(defun first-num (str)
	    (read-from-string (subseq str 0 (search "," str))))

; Return the second number from a comma-separated string of two elements
(defun second-num (str)
	    (read-from-string (subseq str (+ (search "," str) 1))))

; Convert a comma-separated string to a token list
; (this function from rosettacode.org / tokenize a string)
(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

; Build the string to insert a comma-separated list of parking areas into a separate graph
(defun build-area-insert (area-list)
  (format nil "INSERT DATA {
  GRAPH <http://instans.cs.hut.fi/spire_repository/area_list> {
~{ spire_data:arealist park:ParkingArea spire_data:~A .~} } }" (comma-split area-list))
)

; Extracts a single string value from JSON parsed with sparql-response
; If the specified field is not found, NIL is returned
(defun extract-sparql-value (sparql-result field-specifier)
  (cdr (assoc :value
	      (cdr (assoc field-specifier sparql-result)))))

					; If intstring is NIL return zero, otherwise parse integer
(defun parse-integer-or-zero (intstring)
  (if intstring
      (parse-integer intstring)
      0))

; Perform a standard sparql update call with standard prefixes
; Note that the first parameter is expected to be the update query string
(defmacro sparql-update (&rest parameters)
  `(drakma:http-request update-uri :method :post
			:content (concatenate 'string "update=" prefix-str 
					      (format nil ,@parameters))
			))
; This might be needed - but currently doesn't seem so:
;			 :accept update-content-type ; Does not seem necessary - keeping here as a reminder

; Perform a standard sparql query with standard prefixes, return json-string
; First parameter is expected to be the query string
(defmacro sparql-query (&rest parameters)
  `(flexi-streams:octets-to-string (drakma:http-request repository-uri
			:parameters (list (cons "query" (concatenate 'string prefix-str (format nil ,@parameters))))
			:accept query-content-type) :external-format :utf-8))

; Parses query response to the level of containing the response values
; NOTE: Only tested with queries responding with ONE element (which may contain multiple values)
(defmacro parse-sparql-response (&rest parameters)
  `(car (cdr (assoc :bindings (cdr (assoc :results (cl-json:decode-json-from-string (sparql-query ,@parameters)))))))
)

; GRAPHs of spire repository

(defparameter static-global-graph "static_global"
"Static global operation parameters (e.g. timeout limits)")
(defparameter static-user-graph "static_user"
"Static user database")
(defparameter dynamic-user-graph "dynamic_user"
"User data updated dynamically during program execution")
(defparameter static-area-graph "static_area"
"Static parking area database")
(defparameter dynamic-area-graph "dynamic_area"
"Dynamic parking area information")
(defparameter static-destination-graph "static_destination"
"Static information on destinations")
(defparameter log-graph "log"
"System log for all events during execution")

; HTTP parameter strings

(defparameter repository-uri "http://localhost:8080/openrdf-sesame/repositories/spire"
"URI of the Sesame (currently) repository for all triple data")

(defparameter update-uri (concatenate 'string repository-uri "/statements")
"URI of the Sesame (currently) repository SPARQL update commands")

(defparameter gcm-uri "https://android.googleapis.com/gcm/send"
"GCM send URI (according to http://developer.android.com/google/gcm/gcm.html)")

(defparameter gcm-api-key "key=XXxxxXXXXXxxxxxxxxxxxxxxxxxxxxxxx" ; INIT_CONFIG Insert Google API key for Google Cloud Messaging
"API key for Google Cloud Messaging")

(defparameter query-content-type "application/sparql-results+json; charset=utf-8"
"Content type for the data queried from repository")

(defparameter update-content-type "application/x-www-form-urlencoded"
"Content type for SPARQL Updates.")

(defparameter gcm-content-type "application/json"
"Content type for GCM queries.")

; General prefixes

(defparameter prefix-str
"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX geo:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX spire:<http://instans.cs.hut.fi/spire_ontology#>
PREFIX park:<http://spitfire-project.eu/cc/parking>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
PREFIX vso:<http://www.heppnetz.de/ontologies/vso/ns#>
PREFIX spire_data:<http://instans.cs.hut.fi/spire_data#>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX tl: <http://purl.org/NET/c4dm/timeline.owl#>
"
"The usual set of prefixes for SPIRE.")

;; ***********************
;; Profile Check Handler
;; ***********************

(defparameter profilecheck-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:profileCheck ;
   foaf:mbox <mailto:~A> ;
   spire:gcm_reg_id \"~A\" ;
   tl:at \"~A\"^^xsd:dateTime
} }"
"SPARQL Update to INSERT an entry on profile check. Parameters: <graph-string> user gcm-reg-id current-datetime")

(defparameter profilecheck-query
"SELECT ?email
WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {
    ?user a spire:user ;
         foaf:mbox <mailto:~A> ;
         foaf:mbox ?email
  }
}"
"Check, whether the user profile exists in our database - return email address. Parameter: user")

(defparameter sessionstatus-delete 
"DELETE WHERE {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
?statusentry a spire:sessionStatus ;
   foaf:mbox <mailto:~A> ;
   ?pred ?obj
} }"
"SPARQL Update to DELETE previous session status entry of a user. Parameters: <graph-string> user")

(defparameter sessionstatus-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:sessionStatus ;
   spire:sessionStatusValue spire:~A ;
   foaf:mbox <mailto:~A> ;
   spire:gcm_reg_id \"~A\" ;
   tl:at \"~A\"^^xsd:dateTime ;
} }"
"SPARQL Update to INSERT a session status entry. Parameters: <graph-string> status user gcm_reg_id current-datetime")

(hunchentoot:define-easy-handler (profilecheck :uri "/spire/profilecheck") (user gcm_reg_id)
  (setf (hunchentoot:content-type*) query-content-type)
  ; Log profilecheck entry
  (sparql-update profilecheck-insert log-graph user gcm_reg_id (datetime-now))
  ; Check user existence
  ; Did we get the user we queried for?
  (if (search user (sparql-query profilecheck-query user)) 
	(progn
	  ; The user is known
          ; TODO: Check if the user is active - if not, activate user
          ; For the time being - we activate the user anyway
          ; Delete earlier session status entries
          (sparql-update sessionstatus-delete dynamic-user-graph user)
          ; Insert new session status entry
          (sparql-update sessionstatus-insert dynamic-user-graph "Active" user gcm_reg_id (datetime-now))
          ; Send initial window
          (json:encode-json-to-string
            '#( ((viewport_centre . ((lat . (60.184807)) (long . (24.827385)) (zoom . (14)))) (use_defined_point . t)))) ; INIT_CONFIG: Initial viewport address & zoom level
	)
	(progn
	  ; Unknown user - send registration URI
          (json:encode-json-to-string
              '#( (("registration_uri" . "http://0.0.0.0/spire/registration")))) ; INIT_CONFIG: Registration page address
	)
      ) ; end-of-if

   )

;; ****************************
;; Logout
;; ****************************

(defparameter logout-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:logout ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime
} }"
"SPARQL Update to INSERT a logout-entry. Parameters: <graph-string> user current-datetime")

(hunchentoot:define-easy-handler (logout :uri "/spire/logout") (user)
  (setf (hunchentoot:content-type*) query-content-type)
  ; Log the logout entry
  (sparql-update logout-insert log-graph user (datetime-now))
  ; MJR Note: A much more efficient delete-rule was prepared, but Sesame refused to execute it.
  ; One question is, whether gcm registration id:s should be saved after logout? Current one keeps them.
  ; Delete old viewupdate entries of this user from dynamic user data
  (sparql-update viewupdate-delete dynamic-user-graph user)
  ; Replace session status value with Passive & LoggedOut, update time
  (sparql-update sessionstatusvalue-replace dynamic-user-graph "Passive" "LoggedOut" (datetime-now) user)
  ; Delete old destination entries of this user from dynamic user data
  (sparql-update setdestination-delete dynamic-user-graph user)
  (json:encode-json-to-string
     `#( (("logout" . ,user))))
)

;; ****************************
;; Spire User Registration Page
;; ****************************

(hunchentoot:define-easy-handler (registration :uri "/spire/registration") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "SPIRE Registration Page will be made available at this address.")
)

;; ****************************
;; Spire About & Help Page
;; ****************************

(hunchentoot:define-easy-handler (about :uri "/spire/about") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "SPIRE About Page will be made available at this address.")
)


;; ***********************
;; Viewport Update Handler
;; ***********************

; Original version for SPIRE Android client: Logs an email address from the user.

(defparameter viewport-query
  "SELECT (strafter(str(?area_uri),\"#\") AS ?area) ?parkid ?areatype ?information ?lat ?long ?size (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?radius

WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {
    ?usr a spire:user ;
         foaf:mbox <mailto:~A> .
    OPTIONAL { ?usr spire:hasParkingPermit ?usr_permit }
  } # end of static_user

  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    ?area_uri a park:ParkingArea ;
         spire:areaType ?areatype ;
         park:id ?parkid ;
         spire:parkingAreaLocation [ geo:lat ?lat ; geo:long ?long ; ] ;
    BIND (\"~A\" AS ?language)
    FILTER ( lang(?parkid) = ?language )
    OPTIONAL { ?area_uri spire:requiresParkingPermit ?area_permit }
    OPTIONAL { ?area_uri rdfs:comment ?information
         FILTER ( lang(?information) = ?language ) } .
    OPTIONAL { ?area_uri park:size ?size }
    OPTIONAL { ?area_uri spire:parkingAreaRadius ?radius }
  } # end of static_area

  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
  } }
  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
  } } 

  FILTER ( (?area_permit = ?usr_permit) || !bound(?area_permit) )
  FILTER ( ?lat > ~A && ?lat < ~A && ?long > ~A && ?long < ~A )
}
ORDER BY DESC(?size)
LIMIT 20"
  "SPARQL query string to obtain viewport information. Parameters: user lang sw-lat ne-lat sw-long ne-long")

(defparameter viewupdate-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:viewPort ;
    spire:viewPortWindow [ spire:sw [ geo:lat ~A ; geo:long ~A ; ] ;
                           spire:ne [ geo:lat ~A ; geo:long ~A ; ] ; ] ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime
} }"
"SPARQL Update to INSERT an entry on a viewupdate.
 Parameters: <graph-string> sw-lat sw-long ne-lat ne-long user current-datetime")

(defparameter viewupdate-delete 
"DELETE WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
   ?viewport_entry a spire:viewPort ;
     foaf:mbox <mailto:~A> ;
     tl:at ?timestamp ;
     spire:viewPortWindow ?blank1 .
   ?blank1 ?connect ?blank2 .
   ?blank2 ?connect2 ?blank3 } }"
"SPARQL Update to DELETE all of user's viewPort entries. Parameters: <graph-string> user")

(defparameter sessionstatusvalue-replace 
"WITH <http://instans.cs.hut.fi/spire_repository/~A>
DELETE { ?statusentry spire:sessionStatusValue ?oldstatus ;
    spire:sessionStateChangeReason ?reason ;
    tl:at ?oldtime }
INSERT { ?statusentry spire:sessionStatusValue spire:~A ;
    spire:sessionStateChangeReason spire:~A ;
    tl:at \"~A\"^^xsd:dateTime }
WHERE { ?statusentry a spire:sessionStatus ;
    foaf:mbox <mailto:~A> ;
    spire:sessionStatusValue ?oldstatus ;
    tl:at ?oldtime .
    OPTIONAL { ?statusentry spire:sessionStateChangeReason ?reason } }"
"SPARQL Update to REPLACE the session status value. Parameters: <graph-string> status reason current-datetime user")

(hunchentoot:define-easy-handler (viewupdate :uri "/spire/viewupdate") (user sw ne lang)
  (setf (hunchentoot:content-type*) query-content-type)
  (let* ((sw-lat (first-num sw))
	 (sw-long (second-num sw))
	 (ne-lat (first-num ne))
	 (ne-long (second-num ne))
)
    ; Log the viewupdate request
    (sparql-update viewupdate-insert log-graph sw-lat sw-long ne-lat ne-long user (datetime-now))
    ; Note: If there's a need to process with both old & new viewport, it should be done here
    ; Delete old viewupdate entries of this user from dynamic user data
    (sparql-update viewupdate-delete dynamic-user-graph user)
    ; Insert new entry to dynamic user data
    (sparql-update viewupdate-insert dynamic-user-graph sw-lat sw-long ne-lat ne-long user (datetime-now))
    ; Replace session status value with Active & ViewUpdate, update time
    (sparql-update sessionstatusvalue-replace dynamic-user-graph "Active" "ViewUpdate" (datetime-now) user)
    ; Request the view update
    (sparql-query viewport-query user lang sw-lat ne-lat sw-long ne-long)
    ))

;; **************************
;; Public Parking View Update
;; **************************

; New version of viewport update for the new public WebApp:
; No user identification used / needed; completely public access & response.

; This one neither logs the request nor maintains any user status.

(defparameter viewparking-query
  "SELECT DISTINCT (strafter(str(?area_uri),\"#\") AS ?area) ?parkid ?areatype ?information ?lat ?long ?size (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?radius ?imagesavailable

WHERE {
  {
    GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
      ?area_uri a park:ParkingArea ;
           spire:areaType ?areatype ;
           park:id ?parkid ;
           spire:parkingAreaLocation [ geo:lat ?lat ; geo:long ?long ; ] ;
      BIND (\"~A\" AS ?language)
      FILTER ( lang(?parkid) = ?language )
      OPTIONAL { ?area_uri spire:requiresParkingPermit ?area_permit }
      OPTIONAL { ?area_uri rdfs:comment ?information
           FILTER ( lang(?information) = ?language ) } .
      OPTIONAL { ?area_uri park:size ?size }
      OPTIONAL { ?area_uri spire:parkingAreaRadius ?radius }
      OPTIONAL { ?area_uri spire:image ?image_entry .
        BIND (IF (BOUND(?image_entry), true, false) AS ?imagesavailable) }
    } # end of static_area
  } UNION {
    GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {
      ?area_uri a spire:destinationInfo ;
           spire:areaType ?areatype ;
           rdfs:label ?parkid ;
           geo:Point [ geo:lat ?lat ; geo:long ?long ; ] ;
      BIND (\"~A\" AS ?language)
      FILTER ( lang(?parkid) = ?language )
      OPTIONAL { ?area_uri rdfs:comment ?information
           FILTER ( lang(?information) = ?language ) } .
      OPTIONAL { ?area_uri spire:image ?image_entry .
        BIND (IF (BOUND(?image_entry), true, false) AS ?imagesavailable) }
    } # end of static_destination2
  }

  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
  } }
  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
  }  }

  FILTER ( !bound(?area_permit) ) # Pass through the ones with no special permits.
  FILTER ( ?lat > ~A && ?lat < ~A && ?long > ~A && ?long < ~A )
}"
  "SPARQL query string to obtain public parking information. Parameters: lang lang sw-lat ne-lat sw-long ne-long")

(hunchentoot:define-easy-handler (viewparking :uri "/spire/viewparking") (sw ne lang)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (let* ((sw-lat (first-num sw))
	 (sw-long (second-num sw))
	 (ne-lat (first-num ne))
	 (ne-long (second-num ne))
)
    ; Request the view update
    (sparql-query viewparking-query lang lang sw-lat ne-lat sw-long ne-long)
    ))

;; ***********************
;; Destination Image
;; ***********************

; Returns image URIs and coordinates of the specified destination

(defparameter destinationimage-query
  "SELECT ?image ?imglat ?imglong

WHERE {
  {
    GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {
      spire_data:~A a spire:destinationInfo ;
                    spire:image ?image_entry . }
  } UNION {
    GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
      spire_data:~A a park:ParkingArea ;
                    spire:image ?image_entry . }
  }
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {
      ?image_entry a spire:image ;
                   geo:Point [ geo:lat ?imglat ; geo:long ?imglong ; ] ;
                   spire:imageuri ?image
    } # end of static_destination2
}"
  "SPARQL query string to obtain images of a specified destination. Parameters: area area")

(hunchentoot:define-easy-handler (destinationimage :uri "/spire/destinationimage") (area)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  ; Request the destination images
  (sparql-query destinationimage-query area area) )

;; ***********************
;; Walking Guidance
;; ***********************

(defparameter walkingguidance-query
  "SELECT DISTINCT ?label ?comment ?imagesavailable
WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {
    ?instruction a spire:walkingInstructions ;
        park:ParkingArea spire_data:~A ;
        spire:destination spire_data:~A ;
        rdfs:label ?label ;
        rdfs:comment ?comment ;
        spire:image ?image_entry .
    BIND (\"~A\" AS ?language)
    FILTER ( lang(?label) = ?language )
    FILTER ( lang(?comment) = ?language )
    OPTIONAL { ?image_entry a spire:image .
        BIND (IF (BOUND(?image_entry), true, false) AS ?imagesavailable) }
} }"
  "SPARQL query string to return walking guidance information between two destinations. Parameters: from to lang")

(hunchentoot:define-easy-handler (walkingguidance :uri "/spire/walkingguidance") (from to lang)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  ; Request the walking guidance
  (sparql-query walkingguidance-query from to lang)
)

;; ***********************
;; Walking Image
;; ***********************

; Returns the image coordinates and URIs associated with a certain walking guidance entry

(defparameter walkingimage-query
  "SELECT ?image ?imglat ?imglong
WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination2> {
    ?instruction a spire:walkingInstructions ;
        park:ParkingArea spire_data:~A ;
        spire:destination spire_data:~A ;
        spire:image ?image_entry .
    ?image_entry a spire:image ;
        geo:Point [ geo:lat ?imglat ; geo:long ?imglong ; ] ;
        spire:imageuri ?image .
} }"
  "SPARQL query string to return images associated with walking guidance. Parameters: from to")

(hunchentoot:define-easy-handler (walkingimage :uri "/spire/walkingimage") (from to)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  ; Request the images associated with the walking guidance
  (sparql-query walkingimage-query from to)
)

;; ***********************
;; Set Destination
;; ***********************

(defparameter setdestination-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:userDestination ;
   geo:Point [ geo:lat ~A ; geo:long ~A ; ] ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime
} }"
"SPARQL Update to INSERT an entry on destination setting.
 Parameters: <graph-string> lat long user current-datetime")

(defparameter setdestination-delete 
"DELETE WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
   ?destination_entry a spire:userDestination ;
     foaf:mbox <mailto:~A> ;
     tl:at ?timestamp ;
     geo:Point ?blank1 .
   ?blank1 ?connect ?blank2 } }"
"SPARQL Update to DELETE all of user's userDestination entries. Parameters: <graph-string> user")

(defparameter destination-info-query
  "SELECT ?lat ?long ?label ?information
WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination> {
[] a spire:destinationInfo ;
   geo:Point [ geo:lat ~A ; geo:long ~A ; ] ;
   geo:Point [ geo:lat ?lat ; geo:long ?long ; ] ;
   rdfs:label ?label ;
   rdfs:comment ?information ;
   BIND (\"~A\" AS ?language)
   FILTER ( lang(?label) = ?language )
   FILTER ( lang(?information) = ?language )
  } }"
  "SPARQL query to obtain destination information. Parameters: lat long lang")

(hunchentoot:define-easy-handler (setdestination :uri "/spire/setdestination") (user location lang)
  (setf (hunchentoot:content-type*) query-content-type)
  (let* ((lat (first-num location))
	 (long (second-num location)))
    ; Log destination set
    (sparql-update setdestination-insert log-graph lat long user (datetime-now))
    ; Delete old destination entries of this user from dynamic user data
    (sparql-update setdestination-delete dynamic-user-graph user)
    ; Insert new entry to dynamic user data
    (sparql-update setdestination-insert dynamic-user-graph lat long user (datetime-now))
    ; Replace session status value with Active & ViewUpdate, update time
    (sparql-update sessionstatusvalue-replace dynamic-user-graph "Active" "SetDestination" (datetime-now) user)
    ; Search for destination information
    (sparql-query destination-info-query lat long lang)
    ))

;; ***********************
;; Area Info Update
;; ***********************

(defparameter areaupdate-query
  "SELECT (strafter(str(?area_uri),\"#\") AS ?area) ?parkid ?areatype ?information ?lat ?long ?size (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?radius

WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {
    ?usr a spire:user ;
         foaf:mbox <mailto:~A> .
    OPTIONAL { ?usr spire:hasParkingPermit ?usr_permit }
  } # end of static_user

  GRAPH <http://instans.cs.hut.fi/spire_repository/area_list> {
    spire_data:arealist park:ParkingArea ?area_uri
  } 

  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    ?area_uri a park:ParkingArea ;
         spire:areaType ?areatype ;
         park:id ?parkid ;
         spire:parkingAreaLocation [ geo:lat ?lat ; geo:long ?long ; ] ;
    BIND (\"~A\" AS ?language)
    FILTER ( lang(?parkid) = ?language )
    OPTIONAL { ?area_uri spire:requiresParkingPermit ?area_permit }
    OPTIONAL { ?area_uri rdfs:comment ?information
         FILTER ( lang(?information) = ?language ) } .
    OPTIONAL { ?area_uri park:size ?size }
    OPTIONAL { ?area_uri spire:parkingAreaRadius ?radius }
  } # end of static_area

  GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {

    OPTIONAL {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
    }
    OPTIONAL {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
    }
  } # end of dynamic_area

  FILTER ( (?area_permit = ?usr_permit) || !bound(?area_permit) )
}"
  "SPARQL query string to obtain viewport information. Parameters: user area lang area")

(hunchentoot:define-easy-handler (areaupdate :uri "/spire/areaupdate") (user area lang)
  (setf (hunchentoot:content-type*) query-content-type)
  ; Clear the list of areas
  (sparql-update "CLEAR GRAPH <http://instans.cs.hut.fi/spire_repository/area_list>")
  ; Write area list into database
  (sparql-update (build-area-insert area))
  ; Query to find the parking lot info
  (sparql-query areaupdate-query user lang)
    )

;; ***********************
;; New Area Status
;; ***********************

; Subjective parking lot status parameter from human end-user

(defparameter newareastatus-query
"SELECT (strafter(str(?area_uri),\"#\") AS ?area) ?parkid ?areatype ?information ?lat ?long ?size (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?radius

WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {
    ?usr a spire:user ;
         foaf:mbox <mailto:~A> .
    OPTIONAL { ?usr spire:hasParkingPermit ?usr_permit }
  } # end of static_user

  BIND (spire_data:~A AS ?area_uri) #For some reason this binding no longer works in dynamic_area

  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    ?area_uri a park:ParkingArea ;
         spire:areaType ?areatype ;
         park:id ?parkid ;
         spire:parkingAreaLocation [ geo:lat ?lat ; geo:long ?long ; ] ;
    FILTER ( lang(?parkid) = \"~A\" )
    OPTIONAL { ?area_uri spire:requiresParkingPermit ?area_permit }
    OPTIONAL { ?area_uri rdfs:comment ?information
         FILTER ( lang(?information) = \"~A\" ) } .
    OPTIONAL { ?area_uri park:size ?size }
    OPTIONAL { ?area_uri spire:parkingAreaRadius ?radius }
  } # end of static_area

  GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
    BIND (spire_data:~A AS ?area_uri) # Extra binding, because the other one does not persist.

    OPTIONAL {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
    }
    OPTIONAL {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
    }
  } # end of dynamic_area

  FILTER ( (?area_permit = ?usr_permit) || !bound(?area_permit) )
}"
"SPARQL Query to fetch a single area. Parameters: user area lang lang area")

(defparameter newareastatus-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:areaStatusEntry ;
   park:ParkingArea spire_data:~A ;
   park:AreaStatus spire:~A ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime } }"
"SPARQL Update to INSERT new area status based on user update.
 Parameters: <graph-string> area status user current-datetime")

(defparameter newareastatus-delete 
"DELETE WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
?areaentry a spire:areaStatusEntry ;
   park:ParkingArea spire_data:~A ;
   ?property ?value } }"
"SPARQL Update to DELETE previous status entry of the parking lot. Parameters: <graph-string> area")

(defparameter list-users-of-area
"SELECT ?gcm_reg_id
WHERE {
  # Find coordinates of the given parking area
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    BIND ( spire_data:~A as ?area_uri )
    ?area_uri a park:ParkingArea ;
         spire:parkingAreaLocation [
            geo:lat ?lat ;
            geo:long ?long ; ] .
    OPTIONAL { ?area_uri spire:requiresParkingPermit ?area_permit }
  }
  # Find current user viewport window coordinates & user GCM registration ID:s
  GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_user> {
    ?viewport a spire:viewPort ;
      spire:viewPortWindow [ spire:sw [ geo:lat ?sw_lat ; geo:long ?sw_long ; ] ;
                             spire:ne [ geo:lat ?ne_lat ; geo:long ?ne_long ; ] ; ] ;
      foaf:mbox ?user .
    ?status a spire:sessionStatus ;
      spire:sessionStatusValue spire:Active ;
      foaf:mbox ?user ;
      spire:gcm_reg_id ?gcm_reg_id .
  }
  # Find user parking permits
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_user> {
    ?usr a spire:user ;
         foaf:mbox ?user .
         OPTIONAL { ?usr spire:hasParkingPermit ?usr_permit }
  }
  # Pick the users, whose viewports the current parking lot belongs to
  FILTER ( ?lat > ?sw_lat && ?lat < ?ne_lat && ?long > ?sw_long && ?long < ?ne_long )
  # Only return, if the user has the permit to park on the current area
  FILTER ( (?area_permit = ?usr_permit) || !bound(?area_permit) )
}
"
"SPARQL Query to find the GCM registration ID:s of all users, whose current viewport the designated area belongs to. Parameter: area")

; Find the GCM registration ID:s of users, whose viewport the updated area belongs to
(defun notify-areaupdates (area status)
					; gcm-expression gets the query reply in an indented list form
  (let ((gcm-expression (cl-json:decode-json-from-string (sparql-query list-users-of-area area)))
	(regid-list NIL))
    (loop for item in (cdr (assoc :bindings
				  (cdr (assoc :results gcm-expression))))
       for reg = (cdr (assoc :GCM--REG--ID item)) unless (null reg)
       do 
					; Build a comma-separated list of registration_id strings
	 (setq regid-list (concatenate 'string regid-list "\"" (cdr (assoc :value reg)) "\","))
	 ) ; end-of loop
					; Trim final trailing comma (,)
    (setq regid-list (string-right-trim '(#\,) regid-list))
					; http-request to send GCM notifications to corresponding ID:s.
    (if (> (length regid-list) 3) ; Only push if someone is watching
	(drakma:http-request gcm-uri :method :post
			     :additional-headers (list (cons "Authorization" gcm-api-key))
			     :content-type gcm-content-type
			     :content (concatenate 'string 
						   "{ \"collapse_key\": \"" area "\",
\"time_to_live\": 3600,
  \"data\": {
    \"message\": \"area_update_notification\",
    \"area\": \"" area "\",
    \"status\": \"" status "\"},
  \"registration_ids\":[" regid-list "]}" )
			     ) ; end-of GCM http-request
	)
    ) )

; TODO: Restrict this procedure (area status update from human client) to only update the status
; for those areas, which are under manual status monitoring
; For areas with automatic status monitoring, only log the user input.

(hunchentoot:define-easy-handler (newareastatus :uri "/spire/newareastatus") (user area status lang)
  (setf (hunchentoot:content-type*) query-content-type)
					; Log new area status update
    (sparql-update newareastatus-insert log-graph area status user (datetime-now))

					; Delete old dynamic status entries for this destination
    (sparql-update newareastatus-delete dynamic-area-graph area)
					; Insert new entry to dynamic area data
    (sparql-update newareastatus-insert dynamic-area-graph area status user (datetime-now))
    ; Send notifications to impacted users
    (notify-areaupdates area status)
					; Return exactly the same data as for areaupdate
					; Query to find the info for one parking lot
    (sparql-query newareastatus-query user area lang lang area)
    )

;; ******************************
;; Area Occupancy Updates
;; ******************************

; Updates the occupancy figure of a parking area
; Also adjusts the subjective status-parameter

(defparameter anonymous-newareastatus-query
"SELECT (strafter(str(?area_uri),\"#\") AS ?area) (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?size
WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    ?area_uri park:size ?size ;
              spire:sensor \"~A\"
  } # end of static_area

  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
  } }
  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
  } }
}"
"Fetch occupancy information for a single area. Parameters: sensor")

(defparameter newareaoccupancy-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:occupiedSpacesEntry ;
   park:ParkingArea spire_data:~A ;
   park:occupiedSpaces ~A ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime } }"
"SPARQL Update to INSERT new area occupancy based on sensor update.
 Parameters: <graph-string> area occupancy source current-datetime")

(defparameter newareaoccupancy-delete 
"DELETE WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
?areaentry a spire:occupiedSpacesEntry ;
   park:ParkingArea spire_data:~A ;
   ?property ?value } }"
"SPARQL Update to DELETE previous status entry of the parking lot. Parameters: <graph-string> area")

(defun update-area-occupancy (area new-occupancy source)
					; Log new occupancy update
; NOTE: Logging commented out to prevent filling the database - camera-poll has own log implementation
;  (sparql-update newareaoccupancy-insert log-graph area new-occupancy source (datetime-now))

					; Delete old dynamic status entries for this destination
  (sparql-update newareaoccupancy-delete dynamic-area-graph area)
					; Insert new entry to dynamic area data
  (sparql-update newareaoccupancy-insert dynamic-area-graph area new-occupancy source (datetime-now))
  )

					; Updates the stepwise status of a parking area
					; Returns T if the area was found and NIL, if not
					; NOTE: area-size or existence of area are not checked here!
(defun update-area-status (area old-status new-occupancy area-size source)
  (let ((ratio (/ new-occupancy area-size))
	(new-status "Unknown"))
    (if (> ratio 0.999) (setq new-status "Full")
	(if (or (> ratio 0.9) (< (- area-size new-occupancy) 3)) ; >90% full or two places left
	    (setq new-status "AlmostFull")
	    (setq new-status "Available")))
    (if (not (equalp old-status new-status)) ; Status changed
	(progn
					; Log new status update
; NOTE: Logging commented out to prevent filling the database - camera-poll has own log implementation
;	  (sparql-update newareastatus-insert log-graph area new-status source (datetime-now))
					; Delete old dynamic status entries for this destination
	  (sparql-update newareastatus-delete dynamic-area-graph area)
					; Insert new entry to dynamic area data
	  (sparql-update newareastatus-insert dynamic-area-graph area new-status source (datetime-now))
					; Send notifications to impacted users
	  (notify-areaupdates area new-status)))
    ))

(defun update-area-dynamic (source areastatus-response new-occupancy)
  (let ((area-size  (parse-integer-or-zero (extract-sparql-value areastatus-response :size))))
    (if (> area-size 0) ; Was there a corresponding area?
	(progn
; Comment out the border checks below, if unbiased follow-up information on the counters is needed.
	  (if (< new-occupancy 0) (setq new-occupancy 0))
	  (if (> new-occupancy (+ area-size 2)) (setq new-occupancy (+ area-size 2))) ; Setting to max+2 to cope with cars searching for places.
	  (let ((area (extract-sparql-value areastatus-response :area)))
	    (update-area-occupancy area new-occupancy source)
	    (update-area-status area (extract-sparql-value areastatus-response :status) new-occupancy area-size source)))))
  )

;; Set New Area Occupancy

; Sets the occupancy of an area to a number
; Can be used by sensors, which directly give the count of parked cars

(hunchentoot:define-easy-handler (newareaoccupancy :uri "/spire/newareaoccupancy") (source sensor occupancy)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (update-area-dynamic source (parse-sparql-response anonymous-newareastatus-query sensor) (parse-integer-or-zero occupancy))
  (sparql-query anonymous-newareastatus-query sensor)
  )

;; Relative Area Occupancy Change

; Changes the occupancy of an area by +/- integer
; For sensors, which report entries and exits.

(hunchentoot:define-easy-handler (relativeoccupancyupdate :uri "/spire/relativeoccupancyupdate") (source sensor change)
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (let ((areastatus-response (parse-sparql-response anonymous-newareastatus-query sensor)))
    (update-area-dynamic source areastatus-response (+ (parse-integer-or-zero change)
						       (parse-integer-or-zero
							(extract-sparql-value areastatus-response :occupied-spaces)))
			 ) )
  (sparql-query anonymous-newareastatus-query sensor)
  )

;; ******************************
;; View Sensor-Monitored Status
;; ******************************

; Returns the status information of all parking areas monitored with infrastructure sensors

(defparameter viewsensor-query
"SELECT ?sensor (strafter(str(?area_uri),\"#\") AS ?area) ?parkid (strafter(str(?status_uri),\"#\") AS ?status) ?occupiedSpaces ?size
WHERE {
  GRAPH <http://instans.cs.hut.fi/spire_repository/static_area> {
    ?area_uri park:size ?size ;
              park:id ?parkid ;
              spire:sensor ?sensor
  } # end of static_area

  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?area_status_entry a spire:areaStatusEntry ;
         park:ParkingArea ?area_uri ;
         park:AreaStatus ?status_uri
  } }
  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_area> {
      ?occupied_spaces_entry a spire:occupiedSpacesEntry ;
         park:ParkingArea ?area_uri ;
         park:occupiedSpaces ?occupiedSpaces
  } }
}"
"Fetch occupancy information for a single area. Parameters: sensor")

(hunchentoot:define-easy-handler (viewsensor :uri "/spire/viewsensor") ()
  (setf (hunchentoot:content-type*) query-content-type)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (sparql-query viewsensor-query)
  )


;; ***********************
;; Sensor Input Handling
;; ***********************

; TODO note: Add language indicator to http-calls from client so that walking guidance can be returned in the correct language.
; Currently fixed to "eng", since the language was not provided with the original spec.

(defparameter sessionstatus-query 
"SELECT (strafter(str(?status_uri),\"#\") AS ?status) (strafter(str(?reason_uri),\"#\") AS ?reason) (strafter(str(?area_uri),\"#\") AS ?area) ?walkingroute ?walkingguidance

WHERE { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_user> {
  BIND (<mailto:~A> AS ?email)
  ?statusentry a spire:sessionStatus ;
    foaf:mbox ?email .
  OPTIONAL { ?statusentry spire:sessionStatusValue ?status_uri }
  OPTIONAL { ?statusentry spire:sessionStateChangeReason ?reason_uri }
  OPTIONAL { ?statusentry park:ParkingArea ?area_uri }
}
  # Find destination and walking instructions, if parked and any exist
  OPTIONAL { GRAPH <http://instans.cs.hut.fi/spire_repository/dynamic_user> {
     ?userdestination a spire:userDestination ;
       foaf:mbox ?email ;
          geo:Point [ geo:lat ?lat ; geo:long ?long ; ] .
      ?statusentry spire:sessionStateChangeReason spire:Parked 
   }
   GRAPH <http://instans.cs.hut.fi/spire_repository/static_destination> {
     ?destination a spire:walkingInstructions ;
          rdfs:label ?walkingroute ;
          rdfs:comment ?walkingguidance ;
          spire:destinationPoint [ geo:lat ?lat ; geo:long ?long ; ] ;
          park:ParkingArea ?area_uri .
     BIND (\"~A\" AS ?language)
     FILTER ( lang(?walkingroute) = ?language )
     FILTER ( lang(?walkingguidance) = ?language ) }
   }
}"
"SPARQL query to check current session status and return walking guidance, if any. Parameters: user lang")

;; Geofence Crossed

(defparameter geofence-crossed-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:geofenceCrossing ;
   park:ParkingArea spire_data:~A ;
   spire:fenceCrossingDirection spire:~A ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime } }"
"SPARQL Update to INSERT geofence crossed-reports. Parameters: <graph-string> area direction user current-datetime")

(defparameter sessionstatus-parkingarea-delete 
"WITH <http://instans.cs.hut.fi/spire_repository/~A>
DELETE { ?statusentry park:ParkingArea ?area  }
WHERE { 
  ?statusentry a spire:sessionStatus ;
               foaf:mbox <mailto:~A> ;
               park:ParkingArea ?area }"
"SPARQL Update to DELETE the current parking area in a session status entry, if it exists. Parameters: <graph-string> user")

(defparameter sessionstatus-parkingarea-insert 
"WITH <http://instans.cs.hut.fi/spire_repository/~A>
INSERT { ?statusentry park:ParkingArea spire_data:~A }
WHERE { 
  ?statusentry a spire:sessionStatus ;
               foaf:mbox <mailto:~A> }"
"SPARQL Update to INSERT the current parking area to a session status entry. Parameters: <graph-string> area user")

(hunchentoot:define-easy-handler (geofencecrossed :uri "/spire/geofencecrossed") (user area direction)
  (setf (hunchentoot:content-type*) query-content-type)
  ; Log geofence crossing
  (sparql-update geofence-crossed-insert log-graph area direction user (datetime-now))
  ; Delete earlier parking area in session status, if it exists
  (sparql-update sessionstatus-parkingarea-delete dynamic-user-graph user)
  ; Insert the latest parking area. Take 1st area, if multiple given
  (sparql-update sessionstatus-parkingarea-insert dynamic-user-graph (nth 0 (comma-split area)) user)
  ; Query current session status
  (sparql-query sessionstatus-query user "eng")
  )

;; Location Update

(defparameter userlocation-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:locationUpdate ;
   geo:Point [ geo:lat ~A ; geo:long ~A ; ] ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime } }"
"SPARQL Update to INSERT user location reports. Parameters: <graph-string> lat long user current-datetime")

(hunchentoot:define-easy-handler (userlocation :uri "/spire/userlocation") (user location)
  (setf (hunchentoot:content-type*) query-content-type)
  (let* ((lat (first-num location))
	 (long (second-num location))
	 )
    ; Log location updates
    (sparql-update userlocation-insert log-graph lat long user (datetime-now))
    ; Query current session status
    (sparql-query sessionstatus-query user "eng")
    ))
       
;; Activity Change

(defparameter useractivity-insert 
"INSERT DATA {  GRAPH <http://instans.cs.hut.fi/spire_repository/~A> {
[] a spire:activityChange ;
   spire:userActivity spire:~A ;
   foaf:mbox <mailto:~A> ;
   tl:at \"~A\"^^xsd:dateTime } }"
"SPARQL Update to INSERT activity change reports. Parameters: <graph-string> activity user current-datetime")

(hunchentoot:define-easy-handler (useractivity :uri "/spire/useractivity") (user activity)
  (setf (hunchentoot:content-type*) query-content-type)
  ; Log user activity change
  (sparql-update useractivity-insert log-graph activity user (datetime-now))
  ; Temporary simple solution: If we are ON_FOOT, assume we have parked
  ; Next-step solution could be: Save previous activity. If previous=IN_VEHICLE and current=ON_FOOT, assume parked.
  (if (string-equal activity "ON_FOOT") 
      (progn
	 (sparql-update sessionstatusvalue-replace dynamic-user-graph "Passive" "Parked" (datetime-now) user)
	 ; TODO: Improve parking area detection algorithm by taking the parking lot closest to e.g. current coordinates
	 )
      )
  ; Query current session status
  (sparql-query sessionstatus-query user "eng")
)

;********************
; Traffic information
;********************

(hunchentoot:define-easy-handler (traffic :uri "/spire/traffic") (location scale)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (flexi-streams:octets-to-string (drakma:http-request (concatenate 'string
      "http://tieinfo.mustcode.fi/tieinfo/liikenne/maara?json&lat=" (subseq location 0 (search "," location))
      "&lng=" (subseq location (+ (search "," location) 1)) "&scale=" scale))
     :external-format :utf-8))

;*************************
; Road weather information
;*************************

(hunchentoot:define-easy-handler (roadweather :uri "/spire/roadweather") (location scale)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (flexi-streams:octets-to-string (drakma:http-request (concatenate 'string
      "http://tieinfo.mustcode.fi/tieinfo/saa?json&lat=" (subseq location 0 (search "," location))
      "&lng=" (subseq location (+ (search "," location) 1)) "&scale=" scale))
     :external-format :utf-8))


;; **************************************
;; Temporary test notification generators
;; **************************************

; GCM notification tester

(hunchentoot:define-easy-handler (notification :uri "/spire/notification") (registration-id)
  (setf (hunchentoot:content-type*) query-content-type)
  (flexi-streams:octets-to-string (drakma:http-request gcm-uri :method :post
				:additional-headers (list (cons "Authorization" gcm-api-key))
				:content-type gcm-content-type
				:content (concatenate 'string 
"{ \"collapse_key\": \"collapsekey\",
\"time_to_live\": 3600,
  \"data\": {
    \"Test message\": \"Notification test response from SPIRE\"},
  \"registration_ids\":[\"" registration-id "\"]}" )
  ))
)

; New state notification test with GCM

(hunchentoot:define-easy-handler (newstatetest :uri "/spire/newstatetest") (registration-id state reason)
  (setf (hunchentoot:content-type*) query-content-type)
  (flexi-streams:octets-to-string (drakma:http-request gcm-uri :method :post
				:additional-headers (list (cons "Authorization" gcm-api-key))
				:content-type gcm-content-type
				:content (concatenate 'string 
"{ \"collapse_key\": \"new_state_notification\",
\"time_to_live\": 43200,
  \"data\": {
    \"message\": \"new_state_notification\",
    \"state\": \"" state "\",
    \"reason\":\"" reason "\"},
  \"registration_ids\":[\"" registration-id "\"]}" )
  ))
)

; Survey Test

(hunchentoot:define-easy-handler (surveytest :uri "/spire/surveytest") (registration-id survey-uri)
  (setf (hunchentoot:content-type*) query-content-type)
  (flexi-streams:octets-to-string (drakma:http-request gcm-uri :method :post
				:additional-headers (list (cons "Authorization" gcm-api-key))
				:content-type gcm-content-type
				:content (concatenate 'string 
"{ \"collapse_key\": \"launch_survey_notification\",
  \"data\": {
    \"message\": \"launch_survey_notification\",
    \"uri\": \"" survey-uri "\"},
  \"registration_ids\":[\"" registration-id "\"]}" )
  ))
)

; Camera Test

; Generates the same format response as the one agreed with Camera sensor supplier for camera-based entry/exit data

(hunchentoot:define-easy-handler (getevents :uri "/spire/getevents") (lastcounter date key)
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (setf (hunchentoot:content-type*) "application/json")
  "[
	{  \"event\": \"Car enters\",
	   \"sensor\": \"1\",
	   \"counter\": \"17\",
	   \"gate\": \"2\",
	   \"datetime\": \"2014-01-01T12:00:00\"},
	{  \"event\": \"Car enters\",
	   \"sensor\": \"1\",
	   \"counter\": \"18\",
	   \"gate\": \"4\",
	   \"datetime\": \"2014-01-01T12:02:00\"}
]")
