# Powder Sandbox (.NET F# Edition)

A functional .NET 8 counterpart to the [C# Edition](https://github.com/RobertFlexx/Powder-Sandbox-CS-Edition) — both built to bring the classic terminal-based falling-sand simulator into the modern .NET ecosystem.

While the C# version leans on **object-oriented design**, this F# edition embraces **functional purity**, **pattern matching**, and **immutability** to deliver a cleaner, safer, and more mathematically structured sandbox simulation.
Both editions share the same simulation logic and terminal UI — just different philosophies of code.

### See [GameHub](https://github.com/RobertFlexx/Powder-Sandbox-GameHub) for more editions of this game.

---

## Features

* .NET 8 cross-platform (Windows, Linux, macOS)
* Realistic falling-sand simulation — sand, water, lava, smoke, gas, etc.
* Interactive AI — humans and zombies behave intelligently
* Physical interactions — liquids flow, fire burns, gases rise
* Electrical system — lightning and conductive materials react dynamically
* Fully functional design (F# pattern-matching rules for element behavior)
* Console-rendered TUI with colors and menus
* Modular structure — easily extend with new rules or materials

---

## Requirements

* [.NET 8 SDK](https://dotnet.microsoft.com/download/dotnet/8.0)

Check your version:

```bash
dotnet --version
```

Should return something like `8.x.x`

---

## Building and Running

Clone and enter the F# edition folder:

```bash
git clone https://github.com/RobertFlexx/Powder-Sandbox-F-Edition
cd Powder-Sandbox-F-Edition
```

Restore dependencies and run:

```bash
dotnet restore
dotnet run
```

Build for release:

```bash
dotnet build -c Release
```

Publish a standalone binary:

```bash
dotnet publish -r linux-x64 --self-contained true -c Release
```

---

## Controls

| Key               | Action                 |
| ----------------- | ---------------------- |
| Arrow keys / WASD | Move cursor            |
| Space             | Place current element  |
| E                 | Erase with empty space |
| + / -             | Adjust brush size      |
| M / Tab           | Open element menu      |
| P                 | Pause simulation       |
| C / X             | Clear screen           |
| Q                 | Quit simulation        |

---

## Comparison: F# vs. [C# Edition](https://github.com/RobertFlexx/Powder-Sandbox-CS-Edition)

| Aspect            | C# Edition (.NET OOP)                  | F# Edition (.NET Functional)                           |
| ----------------- | -------------------------------------- | ------------------------------------------------------ |
| Programming Style | Object-Oriented (classes, inheritance) | Functional (pattern matching, recursion, immutability) |
| Readability       | Familiar, verbose, traditional syntax  | Concise, expressive, mathematical syntax               |
| Architecture      | Modular classes and methods            | Single-pattern simulation rules                        |
| Performance       | Excellent                              | Excellent (JIT optimized)                              |
| Extensibility     | Easy via classes/interfaces            | Easy via discriminated unions and pattern cases        |
| Safety            | Managed memory, but mutable state      | Strong immutability and type safety                    |
| Learning Curve    | Easier for C/C#/Java devs              | Slightly steeper (functional thinking required)        |
| Future Potential  | Easier GUI/engine integration          | Ideal for rule-based and AI-driven simulation models   |

Both editions share the same simulation logic and controls — the difference lies purely in philosophy: **OOP vs Functional Programming**.

---

## License

Released under the BSD 3-Clause License.

---

## Author

**Robert (@RobertFlexx)**
Creator of FerriteOS, custom shells, editors, and countless terminal simulation projects.

GitHub: [https://github.com/RobertFlexx](https://github.com/RobertFlexx)
