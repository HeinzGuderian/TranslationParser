(in-package :csharp-parser)

(declaim (optimize (debug 3)))

;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))

(defparameter *code-test* 
"public void Start(container as List){
     return \"ArmyEconomy\";} ")

(defparameter *code-test-class* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {}"
)

(defparameter *code-test-class-simple* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI 
{
 private int a;
}"
)

(defparameter *code-test-full* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private List<int> _playerGameObjectList;
      public GameObject _playerGameObject;
      bool _haveConceded = false;
      TeamScript.PlayerNumberEnum _winningPlayer;
      private int a = 3;
      private int b = 3;
      var b = 2;
}
")
