#!/usr/bin/env bb

(require '[clojure.java.shell :as shell]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is are]])

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

(comment
  (deftest format-song-information-test
    (let [current-song (map->CurrentSong {:artist "Snorri Hallgrimsson"
                                          :title "…og minning þín rís hægt (Peter Gregson Rework)"
                                          :position 136
                                          :duration 222})
          expected "Snorri Hallgrimsson : …og minning þín rís hægt (Peter Gregson Rework) (02:16/03:42)"]
      (is (= expected (format-song-information current-song)))))

  (deftest convert-to-time-tests
    (are [exp res] (= exp res)
      "02:16" (convert-to-time 136)
      "03:42" (convert-to-time 222)
      "00:00" (convert-to-time 0)
      "00:10" (convert-to-time 10)
      "00:30" (convert-to-time 30)
      "01:00" (convert-to-time 60)
      "01:03" (convert-to-time 63)))

  (deftest make-current-song-tests
    (let [song-information ["status playing"
                            "file /home/notation/nas/music/Snorri_Hallgrimsson-Orbit_Reworked/01-02-Peter_Gregson-og_minning_bi_n_ri_s_haegt-SMR.flac"
                            "duration 222"
                            "position 136"
                            "tag album Orbit Reworked"
                            "tag title …og minning þín rís hægt (Peter Gregson Rework)"
                            "tag tracknumber 2"
                            "tag discnumber 1"
                            "tag date 2018"
                            "tag genre Électronique"
                            "tag albumartist Snorri Hallgrimsson"
                            "tag artist Snorri Hallgrimsson"
                            "set aaa_mode artist"
                            "set continue true"
                            "set play_library true"
                            "set play_sorted false"
                            "set replaygain disabled"
                            "set replaygain_limit true"
                            "set replaygain_preamp 0.000000"
                            "set repeat false"
                            "set repeat_current false"
                            "set shuffle off"
                            "set softvol false"
                            "set vol_left 100"
                            "set vol_right 100"]
          expected (map->CurrentSong {:artist "Snorri Hallgrimsson"
                                      :title "…og minning þín rís hægt (Peter Gregson Rework)"
                                      :position 136
                                      :duration 222})
          result (make-current-song song-information)]
      (is (= expected result)))

    (let [song-information ["status playing"
                            "file /home/notation/nas/music/thurnin/menhir/Thurnin - Menhir - 02 Ancient Rites.flac"
                            "duration 267"
                            "position 31"
                            "tag title Ancient Rites"
                            "tag artist Thurnin"
                            "tag date 2021"
                            "tag comment Visit https://thurnin.bandcamp.com"
                            "tag album Menhir"
                            "tag tracknumber 2"
                            "tag albumartist Thurnin"
                            "set aaa_mode artist"
                            "set continue true"
                            "set play_library true"
                            "set play_sorted false"
                            "set replaygain disabled"
                            "set replaygain_limit true"
                            "set replaygain_preamp 0.000000"
                            "set repeat false"
                            "set repeat_current false"
                            "set shuffle off"
                            "set softvol false"
                            "set vol_left 100"
                            "set vol_right 100"]
          expected (map->CurrentSong {:artist "Thurnin"
                                      :title "Ancient Rites"
                                      :position 31
                                      :duration 267})
          result (make-current-song song-information)]
      (is (= expected result)))))

(main)
