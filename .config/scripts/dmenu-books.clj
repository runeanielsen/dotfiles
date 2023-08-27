#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.java.shell :as shell]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

(def books-path
  (str (fs/home) "/nas/books/"))

(def book-file-endings
  [".pdf" ".epub"])

(defn all-files-in-path [path]
  (let [directory (io/file path)
        dir? #(.isDirectory %)]
    (map #(.getPath %)
         (filter (comp not dir?)
                 (tree-seq dir? #(.listFiles %) directory)))))

(defn ends-with-any? [s substrs]
  (any? (map #(str/ends-with? s %) substrs)))

(defn open-menu [selections]
  (shell/sh "rofi"
            "-dmenu"
            "-l" "15"
            "-i"
            :in selections))

(defn book-sections [path file-endings]
  (->> path
       all-files-in-path
       (filter #(ends-with-any? % file-endings))
       (map #(str/replace % books-path ""))
       (str/join "\n")))

(defn open-book [path]
  (shell/sh "zathura" path))

(defn main []
  (let [book-paths (book-sections books-path book-file-endings)
        file-to-open (str/replace (:out (open-menu book-paths)) "\n" "")]
    (when (not (str/blank? file-to-open))
      (println (:err (open-book (str books-path file-to-open)))))))

(main)
