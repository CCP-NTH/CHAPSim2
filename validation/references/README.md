# Reference Databases

Reference data is organized by geometry, source, and primary parameter:

```text
references/<geometry>/<source>/<parameter>/
```

Examples:

- `references/channel/mkm/retau180/`
- `references/pipe/tdl/retau550/`
- `references/pipe/eggels/reb5300/`

Keep reference databases separate from generated CHAPSim2 output. Generated
case output belongs in runtime folders such as `1_data/`, `2_visu/`,
`3_monitor/`, and `4_check/`.
