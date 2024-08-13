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
            textMeshPRO.text = "F�r diese Aufgabe k�nnen Sie den Inhalt der T�te Steamboat in den Arbeitsbereich leeren. " +
                " Desweiteren k�nnen Sie die Anleitung Steamboat verdeckt in dem Anleitungsbereich platzieren. Sie haben die exakte Anzahl an Legosteinen, die zur Fertigstellung ben�tigt werden, zur Verf�gung." +
                " Ziel der Aufgabe ist es mit Hilfe der Anleitung das Lego-Modell Steamboat nachzubauen," +
                " sobald Sie diese Erkl�rung schlie�en und den Button Start bet�tigen beginnt die Zeit abzulaufen und Sie k�nnen die Anleitung umdrehen.";
        }
        else if (TaskManager.Instance.task == "Dog")
        {
            textMeshPRO.text = "F�r diese Aufgabe k�nnen Sie den Inhalt aus der T�te 'Dog' in den Arbeitsbereich leeren und die T�te 'Dog Anleitung' unausgepackt in den Anleitungsbereich platzieren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Dog' im Arbeitsbereich mit Hilfe des Lego-Modells im Anleitungsbereich nachzubauen." +
                " Die Zeit startet, sobald Sie diese Erkl�rung geschlossen haben und auf den Button 'Start' dr�cken, dann holen Sie bitten den Inhalt aus der T�te Dog vorsichtig heraus," +
                " sodass das Modell nicht auseinander bricht und starten mit der Konstruktion. Sie haben f�r diese Aufgabe exakt die Menge an Steinen, die Sie ben�tigen, au�erdem entsprechen " +
                " die Farben der Legosteine des Modells denen der Steine im Arbeitsbereich";
        }
        else if (TaskManager.Instance.task == "Nerfgun")
        {
            textMeshPRO.text = "F�r diese Aufgabe k�nnen Sie den Inhalt der T�te 'Nerfgun' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Nerfgun' im Arbeitsbereich nachzubauen. Daf�r wird Ihnen �ber die HoloLens ein Modell eingeblendet. Dieses k�nnen Sie beliebig bewegen, drehen," +
                " vergr��ern oder verkleinern. F�r die Aufgabe haben Sie mehr Steine als Sie ben�tigen zur Verf�gung, au�erdem haben alle Steine die gleiche Farbe." +
                " Die Zeit startet, sobald Sie diese Erkl�rung schlie�en und den Button 'Start' bet�tigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet.";
        }
        else if (TaskManager.Instance.task == "Snake")
        {
            textMeshPRO.text = "F�r diese Aufgabe k�nnen Sie den Inhalt der T�te 'Snake' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Snake' im Arbeitsbereich nachzubauen. Daf�r wird Ihnen �ber die HoloLens ein Modell eingeblendet. Dieses k�nnen Sie beliebig bewegen, drehen," +
                " vergr��ern oder verkleinern. F�r die Aufgabe haben Sie genau so viele Steine wie Sie ben�tigen zur Verf�gung, au�erdem haben alle Steine die gleiche Farbe." +
                " Die Zeit startet, sobald Sie diese Erkl�rung schlie�en und den Button 'Start' bet�tigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet."; 
        }
        else if (TaskManager.Instance.task == "Ferry")
        {
            textMeshPRO.text = "F�r diese Aufgabe k�nnen Sie den Inhalt der T�te 'Ferry' in den Arbeitsbereich leeren." +
                " Ziel der Aufgabe ist es das Lego-Modell 'Ferry' im Arbeitsbereich nachzubauen. Daf�r wird Ihnen �ber die HoloLens ein Modell eingeblendet. Dieses k�nnen Sie beliebig bewegen, drehen," +
                " vergr��ern oder verkleinern. F�r die Aufgabe haben Sie genau so viele Steine wie Sie ben�tigen zur Verf�gung, au�erdem entsprechen die Steine im Arbeitsbereich der Farbe des Lego-Modells." +
                " Die Zeit startet, sobald Sie diese Erkl�rung schlie�en und den Button 'Start' bet�tigen, dann wird Ihnen auch sofort das Lego-Modell eingeblendet.";
        }
        else if (TaskManager.Instance.task == "End")
        {
            textMeshPRO.text = "Danke f�r Ihre Teilnahme an dem Experiment. Sie k�nnen die HoloLens-Brille nun absetzen.";
        }
    }
}
