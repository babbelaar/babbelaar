<div align="center">
  <img alt="Babbelaar-logo" src="./tools/vscode/assets/icon128.png" width="75px" />
  <h1 align="center">Babbelaar: dé Nederlandse programmeertaal!</h1>
  <a href="https://babbelaar.dev/">Website</a> | <a href="https://babbelaar.dev/docs/handleiding/Inleiding/">Handleiding</a> | <a href="https://babbelaar.dev/docs/documentatie/">Documentatie</a> | <a href="https://babbelaar.dev/docs/intern/">Technische documentatie</a>
</div>

## ⚡️ Beginnen
Dit Git-repertoire bevat de code achter de Babbelaar-programmeertaal. Ga voor de documentatie naar <https://babbelaar.dev/>.

## 🤔 Waarom Babbelaar?
* **📚 Gemakkelijk te gebruiken:** Babbelaar is de eerste programmeertaal volledig in het Nederlands. Geen onnodige Engelse termen meer: in Babbelaar schrijf je geen "functions" maar "werkwijzen".
* **🧡 Een ode aan de Nederlandse taal:** Babbelaar maakt programmeren toegankelijker en duidelijker voor Nederlandstaligen. Het viert onze taal in een digitale wereld en doorbreekt taalbarrières in code.
* **⚡️ Razendsnel en efficiënt:** Babbelaar wordt gecompileerd naar machinetaal, waardoor het sneller en efficiënter is dan geïnterpreteerde talen. Dit maakt het ideaal voor toepassingen waar snelheid en prestaties cruciaal zijn.

## 🔨 Bouwstatus
Hieronder zie je de bouwstatussen per platform. Andere platforms die hier niet genoemd zijn, werken mogelijk minder goed of niet.
| Besturingssysteem | Architectuur              | Omgeving  | Status
|-------------------|---------------------------|-----------|--------
| macOS             | AArch64 (Apple Silicon)   | Darwin    | [![Bouwen op macOS (AArch64)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-macos-aarch64.yaml/badge.svg)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-macos-aarch64.yaml)
| macOS             | AMD64 (Intel)             | Darwin    | [![Bouwen op macOS (AMD64)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-macos-amd64.yaml/badge.svg)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-macos-amd64.yaml)
| Ubuntu Linux      | AMD64                     | GNU       | [![Bouwen op Ubuntu Linux (AMD64)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-linux-ubuntu-amd64.yaml/badge.svg)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-linux-ubuntu-amd64.yaml)
| Windows           | AMD64                     | MSVC      | [![Bouwen op Windows (AMD64)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-windows-amd64.yaml/badge.svg)](https://github.com/babbelaar/babbelaar/actions/workflows/bouwen-windows-amd64.yaml)

## 🧑‍💻 Ontwikkeling
Als je mee wilt helpen met het ontwikkelen van Babbelaar, zorg er dan voor dat je de onderstaande pakketten hebt geïnstalleerd:
```sh
sudo npm install -g @vscode/vsce yo typescript
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## 📄 Licentie
Babbelaar is gelicenseerd onder de [Apache License 2.0 (Engelstalig)](https://www.apache.org/licenses/LICENSE-2.0.html). Voor een Nederlandse beschrijving, lees het [Wikipedia-artikel](https://nl.wikipedia.org/wiki/Apache-licentie) erover.

> Copyright 2023 - 2025 Tristan Alexander Gerritsen \
> All Rights Reserved. \
> \
> Licensed under the Apache License, Version 2.0 (the "License"); \
> you may not use this file except in compliance with the License. \
> You may obtain a copy of the License at \
> \
>     http://www.apache.org/licenses/LICENSE-2.0 \
> \
> Unless required by applicable law or agreed to in writing, software \
> distributed under the License is distributed on an "AS IS" BASIS, \
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. \
> See the License for the specific language governing permissions and \
> limitations under the License.
