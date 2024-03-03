(in-package :velopcs)

(defun get-riders-table (url)
  (progn
    (format t "Fetching riders table from ~A~%" url)
    (let* ((request (dex:get url))
           (nodes (lquery:$ (initialize request))))
      (lquery:$ nodes
        (inline (lquery:$ "tr"))
        (inline (lquery:$ "td"))
        (text)))))

(defun parse-string-to-float (str)
  (let ((*read-eval* nil))
    (with-input-from-string (stream str)
      (loop for number = (read stream nil nil)
            while number
            return number))))

(defun slugify (str)
  (let ((slug (str:replace-all "à" "a" (cl-slug:slugify str))))
    (cond ((string-equal slug "jon-barrenetxea") "jon-barrenetxea-golzarri")
          ((string-equal slug "kelland-o-brien") "kelland-brien")
          ((string-equal slug "vinicius-rangel") "vinicius-rangel-costa")
          ((string-equal slug "patrick-eddy") "patrick-eddy2")
          ((string-equal slug "santiago-buitrago") "santiago-buitrago-sanchez")
          ((string-equal slug "pelayo-sanchez") "pelayo-sanchez-mayo")
          ((string-equal slug "eddie-dunbar") "edward-irl-dunbar")
          ((string-equal slug "igor-arrieta") "igor-arrieta-lizarraga")
          ((string-equal slug "jose-manuel-diaz") "jose-manuel-diaz-gallego")
          ((string-equal slug "jokin-murguialday") "jokin-murguialday-elorza")
          ((string-equal slug "pablo-castrillo") "pablo-castrillo-zapater")
          ((string-equal slug "miguel-angel-fernandez") "miguel-angel-fernandez-ruiz")
          ((string-equal slug "unai-iribar") "unai-iribar-jauregi")
          ((string-equal slug "will-barta") "william-barta")
          ((string-equal slug "welay-berhe") "welay-hagos-berhe")
          ((string-equal slug "filippo-magli") "filippo-magli2")
          ((string-equal slug "sinuhe-fernandez") "sinuhe-fernandez-rodriguez")
          ((string-equal slug "guillermo-thomas-silva") "guillermo-thomas-silva-coussan")
          ((string-equal slug "alex-jaime") "alex-jaime-fernandez")
          ((string-equal slug "xavier-canellas") "sanchez-canellas")
          ((string-equal slug "xabier-isasa") "xabier-isasa-larranaga")
          ((string-equal slug "unai-esparza") "unai-esparza-garin")
          ((string-equal slug "negasi-haylu-abreha") "negasi-abreha")
          ((string-equal slug "fernando-tercero") "fernando-tercero-lopez")
          ((string-equal slug "francisco-munoz") "francisco-munoz-llana")
          ((string-equal slug "magnus-cort") "magnus-cort-nielsen")
          ((string-equal slug "lucas-van-boven") "luca-van-boven")
          ((string-equal slug "urko-berrade") "urko-berrade-fernandez")
          ((string-equal slug "pau-miquel") "pau-miquel-delgado")
          ((string-equal slug "jean-louis-le-ny") "jean-louis-le")
          ((string-equal slug "benjamin-thomas") "benjamin-thomas-2")
          ((string-equal slug "jesus-david-pena") "jesus-david-pena-jimenez")
          ((string-equal slug "brandon-smith-rivera") "brandon-smith-rivera-vargas")
          ((string-equal slug "michael-leonard") "michael-leonard1")
          ((string-equal slug "carlos-rodriguez") "carlos-rodriguez-cano")
          ((string-equal slug "samuel-fernandez") "samuel-fernandez-garcia")
          ((string-equal slug "pedro-pinto") "pedro-pinto2")
          ((string-equal slug "guillermo-garcia") "guillermo-garcia-janeiro")
          ((string-equal slug "rafael-barbas") "rafael-elvas-barbas")
          ((string-equal slug "angel-sanchez") "angel-sanchez-rebollido")
          ((string-equal slug "mikkel-frolich-honore") "mikkel-honore")
          ((string-equal slug "jesus-herrada") "jesus-herrada-lopez")
          ((string-equal slug "aadne-holter") "adne-holter")
          ((string-equal slug "daniel-martinez") "daniel-felipe-martinez")
          ((string-equal slug "romain-gregoire") "romain-gregoire1")
          ((string-equal slug "frank-van-den-broeck") "frank-van-den-broek")
          ((string-equal slug "filippo-colombo") "filippo-colombo1")
          ((string-equal slug "kevin-inkelaar") "kevin-inkelaar")
          ;; ((string-equal slug "guillermo-garcia") "guillermo-garcia-janeiro")
          (t slug))))

(defun generate-rider-url (rider-name)
  (str:concat "https://www.procyclingstats.com/rider/"
              (slugify rider-name)
              "/2023"))

(defun search-for-rider (key)
  (dex:get
   (format nil "https://www.procyclingstats.com/search.php?term=~A" key)))

(defun get-rider-url-fuzzy (term)
  (flet ((fetch-url (term)
           (format
            nil
            "https://www.procyclingstats.com/~A/2023"
            (aref
             (let* ((request (search-for-rider term))
                    (nodes (lquery:$ (initialize request))))
               (lquery:$ nodes
                 (inline (lquery:$ "ul.list > li > a"))
                 (attr "href"))) 0))))
    (let* ((url (fetch-url term))
           (new-term (str:concat (first (str:split " " term))
                                 " "
                                 (car (last (str:split " " term))))))
      (if (string= url "https://www.procyclingstats.com/0/2023")
          (fetch-url new-term)
          url))))

(defun check-if-rider-not-found (req)
  (string= "Page not found"
           (let ((nodes (lquery:$ (initialize req))))
             (aref (lquery:$ nodes
                     (inline (lquery:$ "h1"))
                     (text)) 0))))

(defun fetch-rider-page (rider-name)
  (let ((req (dex:get (generate-rider-url rider-name))))
    (if (check-if-rider-not-found req)
        (dex:get (get-rider-url-fuzzy
                  (cl-slug:asciify rider-name)))
        req)))

(defun get-rider-results (rider-name)
  (flet
      ((get-points (req)
         (flet
             ((get-num-string (str)
                (str:trim (second (str:split ":" str)))))
           (let* ((nodes (lquery:$ (initialize req)))
                  (text (lquery:$ nodes
                          ".rdrResultsSum"
                          (text)))
                  (results (str:split "|" (aref text 0))))
             (list :pcs (get-num-string (second results))
                   :uci (get-num-string (third results)))))))
    (let*
        ((fuzzy-search-p nil)
         (pcs-request (fetch-rider-page rider-name))
         (pcs-points (get-points pcs-request)))
      (if (and (getf pcs-points :pcs)
               (getf pcs-points :uci))
          pcs-points
          (if fuzzy-search-p
              (list :pcs "0"
                    :uci "0")
              (progn
                (setf fuzzy-search-p t)
                (setf pcs-request (dex:get
                                   (get-rider-url-fuzzy
                                    (cl-slug:asciify rider-name))))
                (get-points pcs-request)))))))

;; For superclasico, the table is different
;;TODO: try to parse the table head to dynamically get the results
;; (defun get-riders-plist (riders-table)
;;   (let ((riders-table-clean
;;           (loop for i from 0 below (array-dimension riders-table 0)
;;                 when (stringp (aref riders-table i))
;;                   collect (aref riders-table i))))
;;     (mapcar
;;      (lambda (rider-list)
;;        (format t "Fetching rider data for ~A~%" (first rider-list))
;;        (list
;;         :name (first rider-list)
;;         :team (second rider-list)
;;         :velo-points (fifth rider-list)
;;         :results (get-rider-results (first rider-list))))
;;      ;; TODO: check if it's by 5 or 6
;;      (loop for i from 0 below (list-length riders-table-clean) by 6
;;            collect (loop for j from 1 below 6
;;                          collect (nth (+ i j) riders-table-clean))))))/

(defun get-riders-plist (riders-table)
  (let ((riders-table-clean
          (loop for i from 0 below (array-dimension riders-table 0)
                when (stringp (aref riders-table i))
                  collect (aref riders-table i))))
    (mapcar
     (lambda (rider-list)
       (format t "Fetching rider data for ~A~%" (first rider-list))
       (list
        :name (first rider-list)
        :team (second rider-list)
        :velo-points (third rider-list)
        :results (get-rider-results (first rider-list))))
     ;; TODO: check if it's by 5 or 6
     (loop for i from 0 below (list-length riders-table-clean) by 5
           collect (loop for j from 1 below 6
                         collect (nth (+ i j) riders-table-clean))))))/

(defun calculate-weighted-points (riders-plist)
  (loop for rider in riders-plist
        for vp = (parse-string-to-float (getf rider :velo-points))
        for ucip = (or (parse-string-to-float (getf (getf rider :results) :uci)) 0)
        for pcsp = (or (parse-string-to-float (getf (getf rider :results) :pcs)) 0)
        for wpcsp = (/ pcsp vp)
        for wucip = (/ ucip vp)
        collect (list
                 :name (getf rider :name)
                 :team (getf rider :team)
                 :velo-points vp
                 :uci-points ucip
                 :pcs-points pcsp
                 :weighted-uci-points wucip
                 :weighted-pcs-points wpcsp)))

(defun get-all-rider-data (riders-list-url)
  (calculate-weighted-points
   (get-riders-plist
    (get-riders-table riders-list-url))))

;; (defparameter *uae-data* (get-all-rider-data "https://www.velogames.com/uae/2024/riders.php"))
;; (str:to-file "uae-2024.json" (jonathan:to-json *uae-data*))
;; (defparameter *gran-camino-data*
;;   (get-all-rider-data "https://www.velogames.com/gran-camino/2024/riders.php"))
;; (str:to-file "gran-camino-2024.json" (jonathan:to-json *gran-camino-data*))
;; (defparameter *superclasico-data*
;;   (get-all-rider-data "https://www.velogames.com/sixes-superclasico/2024/riders.php"))
;; (str:to-file "superclasico-2024.json" (jonathan:to-json *superclasico-data*))
;; (defparameter *pn-data*
;;   (get-all-rider-data "https://www.velogames.com/pn/2024/riders.php"))
;; (str:to-file "pn-2024.json" (jonathan:to-json *pn-data*))
;; (defparameter *tirreno-data*
;;   (get-all-rider-data "https://www.velogames.com/tirreno-adriatico/2024/riders.php"))
;; (str:to-file "tirreno-2024.json" (jonathan:to-json *tirreno-data*))
