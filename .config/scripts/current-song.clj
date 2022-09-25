#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as str])

(defrecord CurrentSong
    [status
     artist
     title
     position
     duration])

(defn get-song-field [song-information field]
  (when-let [tag (first (filter #(str/starts-with? % field) song-information))]
    (str/replace tag field "")))

(defn make-current-song [song-information]
  (map->CurrentSong
   {:status (or (get-song-field song-information "status ") "")
    :artist (or (get-song-field song-information "tag artist ") "")
    :title (or (get-song-field song-information "tag title ") "")
    :position (Integer. (or (get-song-field song-information "position ") 0))
    :duration (Integer. (or (get-song-field song-information "duration ") 0))}))

(defn format-time [t]
  (let [min (int (/ t 60))
        sec (mod t 60)]
    (format "%02d:%02d" min sec)))

(defn format-song [current-song]
  (if (= (:status current-song) "playing")
    (let [artist (:artist current-song)
          title (:title current-song)
          position (format-time (:position current-song))
          duration (format-time (:duration current-song))]
      (format "%s : %s (%s/%s)" artist title position duration))
    ""))

(defn main []
  (let [song-information (shell/sh "cmus-remote" "-Q")]
    (if (= (:exit song-information) 0)
      (->> (:out song-information)
           (str/split-lines)
           (make-current-song)
           (format-song)
           (println))
      "")))

(main)
