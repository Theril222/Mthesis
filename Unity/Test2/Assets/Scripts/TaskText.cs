using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class TaskText : MonoBehaviour
{
    private TextMeshPro textMeshPRO;
    void Start()
    {
        textMeshPRO = gameObject.GetComponent<TextMeshPro>();
        if (TaskManager.Instance.task == "Steamboat")
        {
            textMeshPRO.text = "Für diese Aufgabe können Sie den Inhalt der Tüte Steamboat in den Arbeitsbereich leeren. " +
                " Desweiteren können Sie die Anleitung Steamboat verdeckt in dem Anleitungsbereich platzieren. Sie haben die exakte Anzahl an Legosteinen, die zur Fertigstellung benötigt werden, zur Verfügung." +
                " Ziel der Aufgabe ist es mit Hilfe der Anleitung das Lego-Modell Steamboat nachzubauen," +
                " sobald Sie diese Erklärung schließen und den Button Start betätigen beginnt die Zeit abzulaufen und Sie können die Anleitung umdrehen.";
        }
        else if (TaskManager.Instance.task == "Dog")
        {
            textMeshPRO.text = "Für diese Aufgabe können Sie den Inhalt aus der Tüte 'Dog' in den Arbeitsbereich leeren und die Tüte 'Dog Anleitung' unausgepackt in den Anleitungsbereich platzieren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Dog' im Arbeitsbereich mit Hilfe des Lego-Modells im Anleitungsbereich nachzubauen." +
                " Die Zeit startet, sobald Sie diese Erklärung geschlossen haben und auf den Button 'Start' drücken, dann holen Sie bitten den Inhalt aus der Tüte Dog vorsichtig heraus," +
                " sodass das Modell nicht auseinander bricht und starten mit der Konstruktion. Sie haben für diese Aufgabe exakt die Menge an Steinen, die Sie benötigen, außerdem entsprechen " +
                " die Farben der Legosteine des Modells denen der Steine im Arbeitsbereich";
        }
        else if (TaskManager.Instance.task == "Nerfgun")
        {
            textMeshPRO.text = "Für diese Aufgabe können Sie den Inhalt der Tüte 'Nerfgun' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Nerfgun' im Arbeitsbereich nachzubauen. Dafür wird Ihnen über die HoloLens ein Modell eingeblendet. Dieses können Sie beliebig bewegen, drehen," +
                " vergrößern oder verkleinern. Für die Aufgabe haben Sie mehr Steine als Sie benötigen zur Verfügung, außerdem haben alle Steine die gleiche Farbe." +
                " Die Zeit startet, sobald Sie diese Erklärung schließen und den Button 'Start' betätigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet.";
        }
        else if (TaskManager.Instance.task == "Snake")
        {
            textMeshPRO.text = "Für diese Aufgabe können Sie den Inhalt der Tüte 'Snake' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Snake' im Arbeitsbereich nachzubauen. Dafür wird Ihnen über die HoloLens ein Modell eingeblendet. Dieses können Sie beliebig bewegen, drehen," +
                " vergrößern oder verkleinern. Für die Aufgabe haben Sie genau so viele Steine wie Sie benötigen zur Verfügung, außerdem haben alle Steine die gleiche Farbe." +
                " Die Zeit startet, sobald Sie diese Erklärung schließen und den Button 'Start' betätigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet."; 
        }
        else if (TaskManager.Instance.task == "Ferry")
        {
            textMeshPRO.text = "Für diese Aufgabe können Sie den Inhalt der Tüte 'Ferry' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Ferry' im Arbeitsbereich nachzubauen. Dafür wird Ihnen über die HoloLens ein Modell eingeblendet. Dieses können Sie beliebig bewegen, drehen," +
                " vergrößern oder verkleinern. Für die Aufgabe haben Sie genau so viele Steine wie Sie benötigen zur Verfügung, außerdem entsprechen die Steine im Arbeitsbereich der Farbe des Lego-Modells." +
                " Die Zeit startet, sobald Sie diese Erklärung schließen und den Button 'Start' betätigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet.";
        }
        else if (TaskManager.Instance.task == "End")
        {
            textMeshPRO.text = "Danke für Ihre Teilnahme an dem Experiment. Sie können die HoloLens-Brille nun absetzen.";
        }
    }
}
