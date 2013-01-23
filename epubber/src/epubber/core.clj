(ns epubber.core
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html])
  (:use [clojure.data.xml]
        [clojure.data.zip.xml]
        [fs.core :exclude [name]]))

;; (require '[clojure.string :as str])
;; (require '[net.cgrand.enlive-html :as html])

(defn src-files []
  (with-mutable-cwd
    (chdir "../src/OEBPS")
    (concat (list-dir *cwd*)
            (list-dir "images"))))

(filter (fn [_] (re-matches #".*\.html" "hi.htm")) '(1 2 3))

(defn media-type [ext]
  (condp = ext
    ".css"  "text/css"
    ".html" "application/xhtml+xml"
    ".gif"  "image/gif"
    ".jpg"  "image/jpeg"
    nil))

;; (media-type ".gif")

(defn parent-dir [file]
  (if (= (name (parent file)) "images")
    "images/"
    ""))

(defn chapter-number [chapter]
  (try
    (Integer. (last (str/split (name chapter) #"-")))
    (catch Exception e
      0)))

;; (chapter-number "book-Z-H-13")

(defn list-elements [source-files & parents]
  (for [file source-files]
    (if (directory? file)
      (list-elements (list-dir file) (concat parents [(name file)]))
      (let [[fname ext] (split-ext file)]
        (element :item {:id fname
                        :href (str (str/join "/" parents)
                                   (if (not (empty? parents)) "/")
                                   file)
                        :media-type (media-type ext)})))))

(defn make-content-opf []
  (let [source-files (src-files)
        images       (filter #(= (extension %) ".gif") source-files)
        chapters     (filter #(= (extension %) ".html") source-files)]
    (element :package {:xmlns "http://www.idpf.org/2007/opf"
                       :xmlns:dc "http://purl.org/dc/elements/1.1/"
                       :xmlns:opf "http://www.idpf.org/2007/opf"
                       :unique-identifier "ISBN"
                       :version "2.0"}
             (element :metadata {}
                      (element :dc:title {} 
                               "Structure and Interpretation of Classical Mechanics")
                      (element :dc:creator {} "Gerald Jay Sussman and Jack Wisdom with Meinhard E. Mayer")
                      (element :dc:identifier {:opf:scheme "ISBN"}
                               "0-262-019455-4")
                      (element :dc:rights {} "Creative Commons Attribution-Noncommercial 3.0 Unported License.")
                      (element :dc:language {} "en-US")
                      (element :dc:source {} "http://mitpress.mit.edu/sites/default/files/titles/content/sicm/book.html")
                      (element :dc:publisher {} "MIT Pres")
                      (element :meta {:name "cover" :content "cover.jpg"}))
             (element :manifest {}
                      (list-elements source-files))

             (element :spine {:toc "ncx"}
                      (for [file (sort-by chapter-number chapters)]
                        (let [idref (name file)]
                          (if (not= (chapter-number file) 0)
                            (element :itemref {:idref idref
                                               :linear "yes"})))))
             
             (element :guide {}
                      (element :reference {:href "book.html"
                                           :type "cover"
                                           :title "Cover"})))))


(def toc-url (file "../src/OEBPS/book-Z-H-4.html"))

(def not-nil? (complement nil?))

(defn toc-filter [selection]
  (filter #(and (not-nil? (get-in % [:attrs :name]))
                (not-nil? (:content %))) selection))

(defn make-toc-vec []
  (let [page (html/html-resource base-url)]
    (toc-filter (html/select page [:a]))))

(defn depth-vector [elem]
  (remove nil?
          (map #(try (Integer. %) (catch Exception e nil))
               (str/split (or 
                           (re-find #"[\d+\.]+" (first (:content elem))) 
                           "")
                          #"\."))))

;; (depth-vector (nth (make-toc-vec) 6))
;; not worth it...

(defn nav-point [elem idx]
  (element :navPoint {:id (str "navPoint-" idx) :playOrder idx}
           (element :navLabel {}
                    (element :text {} (str/replace (first (:content elem))
                                                   " " " ")))
           (element :content {:src (get-in elem [:attrs :href])})))

;; (print (indent-str (nav-point (nth (make-toc-vec) 2) 1)))

(defn make-toc-ncx [toc-data]
  (element :ncx {:xmlns "http://www.daisy.org/z3986/2005/ncx/" 
                 :version "2005-1"}
           (element :head {}
                    (element :meta {:name "dtb:uid" 
                                    :content "us-sicm-db2001"})
                    (element :meta {:name "dtb:depth" :content 4})
                    (element :meta {:name "dtb:totalPageCount" :content 0})
                    (element :meta {:name "dtb:maxPageNumber" :content 0}))
           (element :docTitle {}
                    (element :text {} "Structure and Interpretation of Classical Mechanics"))
           (element :navMap {}
                    (for [i (range (count toc-data))]
                      (nav-point (nth toc-data i) i)))))

(spit "contents.opf" (indent-str (make-content-opf)))

(spit "toc.ncx" (indent-str (make-toc-ncx (make-toc-vec))))
