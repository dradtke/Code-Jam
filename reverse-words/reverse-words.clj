;
; URL: http://code.google.com/codejam/contest/351101/dashboard#s=p1
;

(use '[clojure.string :only (join split)])

(defn solve [line]
  (join " " (reverse (split line #"\s"))))

(loop [n (read-string (read-line)) case 1]
  (println (str "Case #" case ": " (solve (read-line))))
  (if (> n case) (recur n (inc case))))
