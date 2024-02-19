(in-package :velopcs)

(defparameter *app* (make-instance 'ningle:app))

(defparameter *wrapped-app*
  (lack:builder
   (:static
    :path "/static/"
    :root #P"./www/")
   *app*))

;; (defparameter *server*
;;   (clack:clackup *wrapped-app* :port 8888))
;; (clack:stop *server*)

(defparameter *races-list*
  (list
   (list
    :besseges
    "Bessèges 2024"
    (uiop:read-file-string
     #P"~/Documents/projects/velopcs/src/besseges-2024.json"))
   (list
    :down-under
    "Tour Down Under 2024"
    (uiop:read-file-string
     #P"~/Documents/projects/velopcs/src/down-under-2024.json"))
   (list
    :valencia
    "Valencia 2024"
    (uiop:read-file-string
     #P"~/Documents/projects/velopcs/src/valencia-2024.json"))
   (list
    :uae
    "UAE Tour 2024"
    (uiop:read-file-string
     #P"~/Documents/projects/velopcs/src/uae-2024.json"))))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string ()
     (:doctype "html")
     (:html
      (:head
       (:link :rel "icon" :type "image/x-icon" :href "./static/favicon.ico")
       (:meta :charset "utf-8")
       (:meta :name "viewport"
              :content "width=device-width, initial-scale=1")
       (:meta :name "description"
              :content "Velopcs")
       (:script :src "https://cdn.tailwindcss.com")
       (:script :src "https://unpkg.com/htmx.org@1.9.10")
       (:title ,title))
      (:body
       ,@body))))

(defun dropdown ()
  (spinneret:with-html-string ()
    (:div
     (:label :for "races" :class "block mb-2 text-sm font-medium text-gray-900
                                  dark:text-white"
             "Select a race")
     (:select :id "races" :name "race"
       :class "bg-gray-50 border border-gray-300
               text-gray-900 text-sm rounded-lg
               focus:ring-blue-500 focus:border-blue-500
               block w-full p-2.5"
       :data-hx-get "/table"
       :data-hx-target "#table"
       :data-hx-indicator ".htmx-indicator"
       (:option :selected T "Choose a race")
       (loop for (key name data) in *races-list*
             do (:option :value key name))))))

(defun table-headings (race-key)
  (spinneret:with-html-string ()
    (loop for key in (get-p-list-keys
                      (first
                       (jonathan:parse
                        (caddr (assoc race-key *races-list*)))))
          do (:th :scope "coll"
                  :class "text-sm font-medium text-gray-900
                          px-6 py-4 text-left"
                  key))))

(defun table-body (race-key)
  (spinneret:with-html-string ()
    (loop for rider in (jonathan:parse
                        (caddr (assoc race-key *races-list*)))
          for counter from 1 do
            (:tr :class (if (or (zerop counter)
                                (evenp counter))
                            "bg-gray-100 border-b"
                            "bg-white border-b")
                 (loop for value in (get-p-list-values rider)
                       do (:td :class "text-sm text-gray-900 font-light
                                       px-6 py-4 whitespace-nowrap"
                               value))))))

(defun table (race-key)
  (spinneret:with-html-string ()
    (:div :class "flex flex-col"
          (:div :class "py-2 inline-block min-w-full sm:px-6 lg:px-8"
                (:div :class "overflow-hidden"
                      (:table :class "bg-white border-p"
                              (:thead :class "bg-white border-b"
                                      (:tr
                                       (:raw (table-headings race-key))))
                              (:tbody
                               (:raw (table-body race-key)))))))))

(defun app-container ()
  (spinneret:with-html-string ()
    (:div :class "mx-auto max-w-7xl sm:px-6 lg:px-8"
          (:div
           (:raw (dropdown))
           (:div :id "table")))))

(defun get-p-list-keys (plist)
  (reverse (loop for (key value) on plist by #'cddr
                 collect key)))

(defun get-p-list-values (plist)
  (reverse (loop for (key value) on plist by #'cddr
                 collect value)))

(setf (ningle:route *app* "/")
      (with-page
          (:title "Velopcs")
        (:raw (app-container))))

(setf (ningle:route *app* "/table")
      #'(lambda (params)
          (let ((race (cdar params)))
            (alexandria:switch (race :test 'equal)
              ("besseges"
               (table :besseges))
              ("down-under"
               (table :down-under))
              ("valencia"
               (table :valencia))
              ("uae"
               (table :uae))))))
