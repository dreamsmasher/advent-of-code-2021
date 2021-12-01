from pathlib import Path
script_dir = Path(__file__).parent.resolve()
template_path = script_dir / "template.hs"

with open(template_path, encoding='utf-8') as fp:
    template = fp.read()
    for day in range(1, 26):
        body = template.replace('INSERTDAY', str(day))
        path = script_dir.parent.resolve() / 'src' / 'AOC2021' / 'Solutions' / f'Day{day}.hs'
        if not path.exists():
            with open(path, 'w', encoding='utf-8') as wfp:
                wfp.write(body)
