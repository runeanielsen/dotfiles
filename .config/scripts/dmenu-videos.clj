#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[clojure.java.shell :refer [sh]]
         '[clojure.java.io :as io])

(def videos-path
  (str (fs/home) "/nas/videos/"))

(def video-file-endings
  [".webm" ".mp4" ".mkv"])

(defn all-files-in-path [path]
  (let [directory (io/file path)
        dir? #(.isDirectory %)]
    (map #(.getPath %)
         (filter (comp not dir?)
                 (tree-seq dir? #(.listFiles %) directory)))))

(defn ends-with-any? [s substrs]
  (any? (map #(str/ends-with? s %) substrs)))

(defn video-sections [path file-endings]
  (->> path
       all-files-in-path
       (filter #(ends-with-any? % file-endings))
       (map #(str/replace % videos-path ""))
       (str/join "\n")))

(defn open-menu [selections]
  (sh "dmenu"
      "-l" "20"
      "-i"
      "-nf" "#ffffff"
      "-nb" "#222222"
      "-sf" "#222222"
      "-sb" "#ffffff"
      :in selections))

(defn play-video [path]
  (sh "mpv" path))

(defn main []
  (let [video-paths ( video-sections videos-path video-file-endings)
        file-to-open (str/replace (:out (open-menu video-paths)) "\n" "")]
    (when (not (str/blank? file-to-open))
      (println (:err (play-video (str videos-path file-to-open)))))))

(main)
