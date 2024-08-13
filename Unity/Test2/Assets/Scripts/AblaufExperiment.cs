using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AblaufExperiment : MonoBehaviour
{
  
    public GameObject Task;

    public void Start_Experiment()
    {
       
        GameObject task2 = Instantiate<GameObject>(Task, Camera.main.transform);
        task2.transform.parent = null;
        
    }

}
