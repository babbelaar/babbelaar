# Babbelaar
De eerste Nederlandse programmeertaal!

## Kenmerken

### Werkwijzen
Ook wel bekend als `functies`, `methodes`, `subroutines` en `procedures` in andere talen:
```babbelaar
Werkwijze hallo(voornaam: Slinger) {
    schrijf(€"Hallo, {voornaam}!");
}
```

### Statements

#### Stel
```babbelaar
stel mijnCijfer = 10;
```

#### Volg
```babbelaar
volg i in reeks(1, 2) {
    schrijf(i)
}
```

### Vormen
#### Slinger
`"Hallo, wereld!"`

#### Sjabloonslinger
```babbelaar
volg i in reeks (0, 11) {
    schrijf(€"Het getal is nu {i}");
}
```

## Ontwikkeling
Als je mee wilt helpen met het ontwikkelen van Babbelaar, zorg er dan voor dat je de onderstaande pakketten hebt geïnstalleerd:
```sh
sudo npm install -g @vscode/vsce yo typescript
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
