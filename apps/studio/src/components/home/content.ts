import channelsSource from '../../assets/examples/channels.vo?raw';
import closuresSource from '../../assets/examples/closures.vo?raw';
import deferSource from '../../assets/examples/defer.vo?raw';
import dynamicAccessSource from '../../assets/examples/dynamic_access.vo?raw';
import errorHandlingSource from '../../assets/examples/error_handling.vo?raw';
import filesystemSource from '../../assets/examples/filesystem.vo?raw';
import goroutinesSource from '../../assets/examples/goroutines.vo?raw';
import interfacesSource from '../../assets/examples/interfaces.vo?raw';
import regexpSource from '../../assets/examples/regexp.vo?raw';
import selectSource from '../../assets/examples/select.vo?raw';
import timeSource from '../../assets/examples/time.vo?raw';

export type StarterCategoryId = 'essentials' | 'concurrency' | 'data';
export type StarterAccent = 'blue' | 'violet' | 'mint' | 'amber';
export type BlockKartAction = 'play' | 'source';

export const BLOCKKART_GITHUB_URL = 'https://github.com/vo-lang/BlockKart';

export interface StarterCategory {
  id: StarterCategoryId;
  label: string;
}

export interface StarterExample {
  id: string;
  title: string;
  file: string;
  description: string;
  source: string;
  category: StarterCategoryId;
  accent: StarterAccent;
  monogram: string;
  tags: readonly string[];
  hasGui: boolean;
}

export const WELCOME_CONTENT = {
  eyebrow: 'The scripting language for the Rust ecosystem',
  title: 'Write with clarity. Run wherever your project lives.',
  description:
    'Vo pairs a familiar, Go-shaped surface with a Rust-native compiler and runtime. Embed it, run it on the VM or JIT, and bring the same code to the browser with WASM.',
  note: 'Experimental, open source, and ready to explore.',
  repositoryUrl: 'https://github.com/vo-lang/volang',
  code: `func main() {
    jobs := make(chan int, 3)

    go func() {
        for id := 1; id <= 3; id++ {
            jobs <- id
        }
        close(jobs)
    }()

    for id := range jobs {
        println("job", id, "ready")
    }
}`,
  capabilities: [
    { label: 'Go-shaped', detail: 'Low-ceremony syntax' },
    { label: 'Rust-native', detail: 'Compiler and runtime' },
    { label: 'VM · JIT · WASM', detail: 'Choose the right backend' },
  ],
} as const;

export const STARTER_CATEGORIES: readonly StarterCategory[] = [
  { id: 'essentials', label: 'Language essentials' },
  { id: 'concurrency', label: 'Concurrency' },
  { id: 'data', label: 'Data & standard library' },
];

export const STARTER_EXAMPLES: readonly StarterExample[] = [
  {
    id: 'error-handling',
    title: 'Error handling',
    file: 'error_handling.vo',
    description: 'Propagate failures with the ? operator and keep cleanup close to the work.',
    source: errorHandlingSource,
    category: 'essentials',
    accent: 'amber',
    monogram: 'ER',
    tags: ['?', 'fail'],
    hasGui: false,
  },
  {
    id: 'closures',
    title: 'Closures',
    file: 'closures.vo',
    description: 'Capture values and build compact, composable behavior.',
    source: closuresSource,
    category: 'essentials',
    accent: 'violet',
    monogram: 'FN',
    tags: ['functions', 'capture'],
    hasGui: false,
  },
  {
    id: 'interfaces',
    title: 'Interfaces',
    file: 'interfaces.vo',
    description: 'Use structural contracts to keep callers flexible and implementations small.',
    source: interfacesSource,
    category: 'essentials',
    accent: 'blue',
    monogram: 'IF',
    tags: ['types', 'methods'],
    hasGui: false,
  },
  {
    id: 'defer',
    title: 'Defer',
    file: 'defer.vo',
    description: 'Schedule predictable LIFO cleanup next to the resource it protects.',
    source: deferSource,
    category: 'essentials',
    accent: 'mint',
    monogram: 'DF',
    tags: ['cleanup', 'control flow'],
    hasGui: false,
  },
  {
    id: 'channels',
    title: 'Channels',
    file: 'channels.vo',
    description: 'Coordinate producers and consumers with typed message passing.',
    source: channelsSource,
    category: 'concurrency',
    accent: 'blue',
    monogram: 'CH',
    tags: ['chan', 'messages'],
    hasGui: false,
  },
  {
    id: 'goroutines',
    title: 'Goroutines',
    file: 'goroutines.vo',
    description: 'Launch lightweight concurrent work with a familiar go expression.',
    source: goroutinesSource,
    category: 'concurrency',
    accent: 'mint',
    monogram: 'GO',
    tags: ['go', 'concurrency'],
    hasGui: false,
  },
  {
    id: 'select',
    title: 'Select',
    file: 'select.vo',
    description: 'Wait on multiple channel operations without coupling their producers.',
    source: selectSource,
    category: 'concurrency',
    accent: 'violet',
    monogram: 'SL',
    tags: ['select', 'channels'],
    hasGui: false,
  },
  {
    id: 'dynamic-access',
    title: 'Dynamic access',
    file: 'dynamic_access.vo',
    description: 'Navigate runtime-shaped values while keeping intent explicit.',
    source: dynamicAccessSource,
    category: 'data',
    accent: 'violet',
    monogram: 'DY',
    tags: ['dynamic', 'data'],
    hasGui: false,
  },
  {
    id: 'filesystem',
    title: 'Filesystem',
    file: 'filesystem.vo',
    description: 'Read, write, and inspect files with the standard library.',
    source: filesystemSource,
    category: 'data',
    accent: 'amber',
    monogram: 'FS',
    tags: ['files', 'stdlib'],
    hasGui: false,
  },
  {
    id: 'regexp',
    title: 'Regular expressions',
    file: 'regexp.vo',
    description: 'Match and transform text with familiar pattern tools.',
    source: regexpSource,
    category: 'data',
    accent: 'blue',
    monogram: 'RX',
    tags: ['text', 'regexp'],
    hasGui: false,
  },
  {
    id: 'time',
    title: 'Time',
    file: 'time.vo',
    description: 'Work with durations, timers, and scheduled operations.',
    source: timeSource,
    category: 'data',
    accent: 'mint',
    monogram: 'TM',
    tags: ['timers', 'stdlib'],
    hasGui: false,
  },
];

export const BLOCKKART_CONTENT = {
  eyebrow: 'Featured project',
  title: 'BlockKart',
  description:
    'Take a quick lap through a 3D kart racer built with Vo. It brings modules, realtime input, a GUI HUD, and browser rendering together in one playable project.',
  repositoryUrl: BLOCKKART_GITHUB_URL,
  tags: ['3D', 'Vo GUI', 'Browser runner'],
} as const;
