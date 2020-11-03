(ns katas.rpg
  "Source: https://github.com/ardalis/kata-catalog/blob/master/katas/RPG%20Combat.md"
  )

; so far: iteration 3

(defn create-uuid [] (str (java.util.UUID/randomUUID)))

(defn create-character [fighter-type]
  {
   :uuid         (create-uuid)
   :type         :human
   :health       1000
   :level        1
   :alive        true
   :fighter-type fighter-type
   :fractions    #{}
   })

(defn create-thing []
  {
   :type   :thing
   :health 1000
   :alive  true
   })

(defn join-fraction [character fraction]
  (update character :fractions #(conj % fraction)))

(defn leave-fraction [character fraction]
  (update character :fractions #(disj % fraction)))

(defn level-up [character]
  (update character :level inc))

(defn get-range [fighter-type]
  (get fighter-type {:melee 2 :ranged 20}))

(defn is-same-character? [first second]
  (= (:uuid first) (:uuid second)))

(defn calculate-damage [attacker-level attacked-level original-damage]
  (cond
    (> (- attacker-level attacked-level) 5) (+ original-damage (* original-damage 0.5))
    (> (- attacked-level attacker-level) 5) (- original-damage (* original-damage 0.5))
    :else original-damage))

(defn is-in-range? [fighter-type distance]
  (>= (get-range fighter-type) distance))

(defn are-allies? [first-character second-character]
  (some #(contains? (:fractions second-character) %) (:fractions first-character)))

(defn attack [attacker character-to-attack damage distance]
  (cond
    (= :thing (:type attacker)) character-to-attack
    (is-same-character? attacker character-to-attack) character-to-attack
    (not (is-in-range? (:fighter-type attacker) distance)) character-to-attack
    (are-allies? attacker character-to-attack) character-to-attack
    :else
    (let [actual-damage (calculate-damage (:level attacker) (:level character-to-attack) damage)
          new-health (- (:health character-to-attack) actual-damage)
          updated-character (assoc character-to-attack :health new-health)
          ]
      (if (<= new-health 0)
        (assoc updated-character :alive false :health 0)
        updated-character))))

(defn heal [healer character-to-be-healed health-points]
  (cond
    (not (:alive character-to-be-healed)) character-to-be-healed
    (not (or (is-same-character? healer character-to-be-healed)
             (are-allies? healer character-to-be-healed))) character-to-be-healed
    :else (assoc character-to-be-healed :health (min 1000 (+ (:health character-to-be-healed) health-points)))))
