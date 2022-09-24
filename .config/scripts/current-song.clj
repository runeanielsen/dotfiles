#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as str])

(defn information-song-field [song-information field]
  (when-let [tag (first (filter #(str/starts-with? % field) song-information))]
    (str/replace tag field "")))

(defrecord CurrentSong
    [status
     artist
     title
     position
     duration])

(defn make-current-song [song-information]
  (map->CurrentSong
   {:status (or (information-song-field song-information "status ") "")
    :artist (or (information-song-field song-information "tag artist ") "")
    :title (or (information-song-field song-information "tag title ") "")
    :position (Integer. (or (information-song-field song-information "position ") 0))
    :duration (Integer. (or (information-song-field song-information "duration ") 0))}))

(defn get-song-information! []
  (shell/sh "cmus-remote" "-Q"))

(defn convert-to-time [t]
  (let [min (int (/ t 60))
        sec (mod t 60)]
    (format "%02d:%02d" min sec)))

(defn format-song-information [current-song]
  (if (= (:status current-song) "playing")
    (let [artist (:artist current-song)
          title (:title current-song)
          position (convert-to-time (:position current-song))
          duration (convert-to-time (:duration current-song))]
      (format "%s : %s (%s/%s)" artist title position duration))
    ""))

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
