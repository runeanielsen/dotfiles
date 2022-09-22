#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as str])

(defn information-song-field [song-information field]
  (-> (filter #(str/starts-with? % field) song-information)
      (first)
      (str/replace field "")))

(defrecord CurrentSong
    [artist
     title
     position
     duration])

(defn make-current-song [song-information]
  (map->CurrentSong
   {:artist (information-song-field song-information "tag artist ")
    :title (information-song-field song-information "tag title ")
    :position (Integer. (information-song-field song-information "position "))
    :duration (Integer. (information-song-field song-information "duration "))}))

(defn get-song-information! []
  (shell/sh "cmus-remote" "-Q"))

(defn convert-to-time [t]
  (let [min (int (/ t 60))
        sec (mod t 60)]
    (format "%02d:%02d" min sec)))

(defn format-song-information [current-song]
  (let [artist (:artist current-song)
        title (:title current-song)
        position (convert-to-time (:position current-song))
        duration (convert-to-time (:duration current-song))]
    (format "%s : %s (%s/%s)" artist title position duration)))

(defn get-current-song-formatted []
  (let [song-information (get-song-information!)]
    (if (= (:exit song-information) 0)
      (->> (:out song-information)
           (str/split-lines)
           (make-current-song)
           (format-song-information))
      "")))

(defn main []
  (println (get-current-song-formatted)))

(main)
