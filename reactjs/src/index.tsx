import * as React from 'react';
import { createRoot } from 'react-dom/client';
import { Day05 } from './Day05/Day05';

const container = document.getElementById('root');
const root = createRoot(container!);
root.render(<Day05 />);
