# Lorenzo Braga — Academic portfolio

Sito statico per GitHub Pages: HTML e un unico foglio CSS, senza dipendenze,
JavaScript o compilazione. Si può aprire `index.html` direttamente nel browser.

## Struttura

| File | Contenuto da aggiornare |
| --- | --- |
| `index.html` | Presentazione, affiliazione, quadro sintetico della formazione |
| `research.html` | Profilo scientifico, INF1, CPZ2, PROTO, BRAIN e ruoli scientifici |
| `publications.html` | Articoli, manoscritti, software, proceedings e capitoli |
| `conf-works.html` | Oral presentations, poster, organizzazione e formazione completa |
| `edu-out.html` | Teaching, tutoring, seminari accademici e public engagement |
| `cv.html` | Education, esperienze, grants, awards, metodi, lingue e memberships |
| `contacts.html` | Email e profili accademici |
| `css/style.css` | Palette, layout, navigazione, componenti, mobile e stampa |
| `CONTENT_REVIEW.md` | Punti da confermare prima dell'aggiornamento finale dei contenuti |

## Come modificare il sito

- L'indentazione è di **due spazi**, senza tab.
- Le sezioni HTML hanno commenti con il loro nome.
- Le attività sono blocchi `<article class="entry">`; le citazioni sono blocchi
  `<p class="citation">`. Copiare un blocco vicino è sufficiente per aggiungere
  una voce nello stesso formato.
- I colori e la larghezza del sito si modificano nelle variabili `:root` del CSS.
- Il menu è scritto in ogni pagina per mantenere il sito leggibile e utilizzabile
  anche senza JavaScript. Se si aggiunge o rinomina una pagina, aggiornare i sette
  menu. `aria-current="page"` deve restare soltanto sul link della pagina corrente.
- Ogni nuovo `id` deve essere unico nella sua pagina. Aggiornare anche il link
  nella navigazione interna quando si rinomina una sezione.
- Le descrizioni nei tag `<meta name="description">` sono specifiche per pagina.
- Sono mantenuti i nomi delle sei pagine originali e gli anchor di sezione utili.
- Immagini e icone restano nelle posizioni originali.

## Separazione portfolio / application CV

Il sito conserva il record esteso delle attività scientifiche. La pagina
`cv.html` fornisce un quadro generale con collegamenti agli elenchi completi.
Il futuro CV per candidature sarà un documento selettivo, costruito a partire
da questo record. In `cv.html` è presente un commento che indica dove inserire
il download del nuovo PDF quando sarà pronto; non c'è un link fittizio.

## Applicazione a GitHub Pages

Questa versione conserva la struttura statica della repo originale.
Sostituire i sei HTML, `css/style.css` e `README.md`; aggiungere `cv.html` e
`CONTENT_REVIEW.md`. Non servono nuovi workflow o impostazioni di hosting.
Le immagini e le icone incluse nell'archivio sono le originali.

Il refactoring è preparato su una copia della repo pubblica e non è stato
pubblicato su GitHub. Non sono state modificate le impostazioni del sito live.

## Verifica

Controllati struttura HTML, unicità degli ID, destinazioni dei link locali e
anchor, presenza delle risorse, uniformità dei menu e copertura della migrazione.
La verifica visiva in browser resta da eseguire: il refactoring include regole
responsive e per la stampa, ma non è stato sottoposto a un controllo visuale.
