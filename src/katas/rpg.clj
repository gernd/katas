(ns katas.rpg
  "Source: https://github.com/ardalis/kata-catalog/blob/master/katas/RPG%20Combat.md"
  )

; so far: iteration 1

(defn create-character []
  {:health 1000
   :level  1
   :alive  true
   })

(defn attack [character-to-attack damage]
  (let [new-health (- (:health character-to-attack) damage)
        updated-character (assoc character-to-attack :health new-health)
        ]
    (if (<= new-health 0)
      (assoc updated-character :alive false :health 0)
      updated-character
      )))

(defn heal [character-to-be-healed health-points]
  (if-not (:alive character-to-be-healed)
    character-to-be-healed
    (assoc character-to-be-healed :health (min 1000 (+ (:health character-to-be-healed) health-points)))
    )
  )
