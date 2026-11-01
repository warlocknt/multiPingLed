# multiPingLed

![License](https://img.shields.io/badge/license-GPLv3-blue.svg)
![Platform](https://img.shields.io/badge/platform-Windows-lightgrey.svg)
![Built with](https://img.shields.io/badge/built%20with-FreePascal%20%2F%20Lazarus-orange.svg)
![Last Commit](https://img.shields.io/github/last-commit/YOUR_USERNAME/multiPingLed)

Lightweight Windows tray utility for monitoring multiple hosts via ICMP (ping) with LED-style status indicators.

---
🇬🇧 [English version](#english-version)
---

# Русская версия

multiPingLed — лёгкая утилита для мониторинга нескольких узлов по ICMP с отображением статуса в системном трее Windows.

Разработано на **FreePascal / Lazarus (LCL)**.  
Не требует внешних зависимостей.

[⬆ Наверх](#multipingled)

---

## ✨ Возможности

- Мониторинг нескольких хостов
- Группировка узлов
- Динамический tray-значок (LED-индикация)
- Многострочный tooltip
- Конфигурация в формате INI
- Валидация конфигурации при запуске
- Экспорт / импорт конфигурации
- Отдельный консольный тест-проект
- GUI создаётся программно при старте

---

## Особенности

- **Отображение в системном трее**: Иконки с LED-индикаторами состояния узлов
- **Группы узлов**:
  - Single: 1 узел
  - 2x2 Grid: 2-4 узла
  - 3x3 Grid: 5-9 узлов
- **Настраиваемые параметры**:
  - Интервал пингования (в миллисекундах)
  - Таймаут ответа
  - Имя и адрес узла
- **Поддержка мультиязычности**: Русский, английский, немецкий, французский и казахский языки
- **Экспорт/Импорт конфигурации**: В формате INI
- **Автоматическое восстановление**: Приложение автоматически создает конфигурацию по умолчанию при первом запуске

## Требования

- Windows 7/8/10/11

 Для компиляции из исходников:
- Lazarus IDE ()
- Free Pascal Compiler (FPC)

## Установка

### Готовый исполняемый файл

1. Скачайте последнюю версию из раздела Releases
2. Распакуйте архив в любую папку
3. Запустите `multiPingLed.exe`

### Компиляция из исходников

1. Откройте проект в Lazarus IDE
2. Скомпилируйте проект (Ctrl+F9)
3. Исполняемый файл будет создан в папке проекта

## Использование

### Первый запуск

При первом запуске приложение автоматически создаст конфигурацию с 14 предустановленными DNS-серверами:
- Cloudflare (1.1.1.1, 1.0.0.1)
- Google (8.8.8.8, 8.8.4.4)
- Quad9 (9.9.9.9, 149.112.112.112)
- OpenDNS (208.67.222.222, 208.67.220.220)
- И другие

### Управление

- **Правый клик по иконке в трее** — открыть меню (Настройки / Выход)
- **Левый клик по иконке** — открыть подсказку со статусом узлов

### Настройка

1. Нажмите правой кнопкой на иконку в трее
2. Выберите "Настройки"
3. Вкладка "Узлы":
   - Добавляйте, редактируйте и удаляйте узлы
   - Укажите имя, IP-адрес/домен, интервал и таймаут
4. Вкладка "Группы":
   - Создавайте группы узлов
   - Выбирайте тип группы (Single, 2x2, 3x3)
   - Выбирайте узлы для группы (2-4 для 2x2, 5-9 для 3x3)
5. Выберите язык интерфейса (русский/английский)
6. Нажмите "Применить" для сохранения

### Цвета индикаторов

- **Зеленый** — узел доступен
- **Красный** — узел недоступен
- **Серый/Желтый** — ожидание первого пингования

## Файлы конфигурации

- `config.ini` — файл конфигурации (создается автоматически)
- `lang\english.txt` — английские переводы
- `lang\russian.txt` — русские переводы
- `lang\german.txt` — немецкие переводы
- `lang\french.txt` — французские переводы
- `lang\kazakh.txt` — казахские переводы

Конфигурация сохраняется в `%APPDATA%\multiPingLed\config.ini`

## Локализация

Для добавления нового языка:

1. Создайте файл `lang\[код_языка].txt`
2. Добавьте строки в формате:
   ```
   code=[код_языка]
   name=[Название языка]
   
   key=value
   ```
3. Перезапустите приложение

## Лицензия

Проект распространяется под лицензией
GNU General Public License v3 (GPLv3)

Разрешено:
  Коммерческое использование
  Форки
  Модификация
  Распространение
  При распространении изменённой версии необходимо предоставить исходный код и сохранить GPLv3.

## Благодарности

- Разработано с использованием Lazarus IDE и Free Pascal
- Вдохновлено необходимостью мониторинга сетевой инфраструктуры

# English version

🇷🇺 [Русская версия](#русская-версия)  

multiPingLed — lightweight Windows tray utility for monitoring multiple hosts via ICMP (ping) with LED-style status indicators.

Developed in **FreePascal / Lazarus (LCL)**.  
No external dependencies required.

[⬆ Back to top](#multipingled)

---

## ✨ Features

- Monitor multiple hosts
- Group hosts
- Dynamic tray icon (LED indicators)
- Multi-line tooltip (for 3x3 groups only host names are shown)
- Configuration in INI format
- Configuration validation on startup
- Export / import configuration
- Separate console test project
- GUI created programmatically at startup

---

## Details

- **System tray display**: Icons with LED indicators for host status
- **Host groups**:
- Single: 1 host
- 2x2 Grid: 2-4 hosts
- 3x3 Grid: 5-9 hosts (tooltip only shows host names)
- **Customizable parameters**:
- Ping interval (ms)
- Response timeout
- Host name and address
- **Multilanguage support**: Russian, English, German, French, Kazakh
- **Export/Import configuration**: INI format
- **Automatic recovery**: App creates default config on first run

## Requirements

- Windows 7/8/10/11

To build from source:
- Lazarus IDE
- Free Pascal Compiler (FPC)

## Installation

### Prebuilt executable

1. Download the latest release from Releases section
2. Extract archive to any folder
3. Run `multiPingLed.exe`

### Build from source

1. Open the project in Lazarus IDE
2. Compile the project (Ctrl+F9)
3. Executable will appear in the project folder

## Usage

### First launch

On first run the app automatically creates a configuration with 14 preconfigured DNS servers:
- Cloudflare (1.1.1.1, 1.0.0.1)
- Google (8.8.8.8, 8.8.4.4)
- Quad9 (9.9.9.9, 149.112.112.112)
- OpenDNS (208.67.222.222, 208.67.220.220)
- And others

### Controls

- **Right click on tray icon** — open menu (Settings / Exit)
- **Left click on tray icon** — show tooltip with host status

### Configuration

1. Right-click tray icon
2. Select "Settings"
3. Hosts tab:
- Add, edit, remove hosts
- Set name, IP/domain, interval, timeout
4. Groups tab:
- Create host groups
- Choose group type (Single, 2x2, 3x3)
- Select hosts for group (2-4 for 2x2, 5-9 for 3x3)
5. Choose UI language (Russian / English)
6. Click "Apply" to save

### LED colors

- **Green** — host is up
- **Red** — host is down
- **Gray/Yellow** — waiting for first ping

## Configuration files

- `config.ini` — configuration file (created automatically)
- `lang\english.txt` — English translation
- `lang\russian.txt` — Russian translation
- `lang\german.txt` — German translation
- `lang\french.txt` — French translation
- `lang\kazakh.txt` — Kazakh translation

Config is saved at `%APPDATA%\multiPingLed\config.ini`

## Localization

To add a new language:

1. Create file `lang\[lang_code].txt`
2. Add lines in format:
   ```
   code=[lang_code]
   name=[Language name]

   key=value

   ```

3. Restart the app

## License

Distributed under GNU General Public License v3 (GPLv3)

Allowed:
Commercial use  
Forks  
Modification  
Redistribution  
When distributing modified versions, source code must be provided and GPLv3 preserved.

## Acknowledgements

- Developed using Lazarus IDE and Free Pascal
- Inspired by the need for network infrastructure monitoring
## История сессий

SQLite встроена в EXE. История хранится в `history.sqlite`, отчёты — в `reports/`, копии повреждённых баз — в `bak/`. Автоматическое создание HTML при выходе включается в общих настройках. Подробности: [SESSION-HISTORY.md](SESSION-HISTORY.md).

Подробности нумерации всех сборок и журнал попыток: [BUILDING.md](BUILDING.md).

## Единственный экземпляр

Повторный запуск в той же сессии Windows показывает предупреждение «Приложение multiPingLED уже запущено» и после нажатия ОК завершается. Работающий экземпляр продолжает мониторинг. Обычная, отладочная и переименованные копии используют один именованный mutex. При аварийном завершении Windows освобождает его автоматически. Язык предупреждения читается из INI без запуска мониторинга, открытия истории или изменения настроек.
