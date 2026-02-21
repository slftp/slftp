import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  build: {
    chunkSizeWarningLimit: 600,
  },
  server: {
    proxy: {
      '/api': {
        target: 'http://127.0.0.1:8089',
        changeOrigin: true,
        // rewrite: (path) => path.replace(/^\/api/, ''), // Don't rewrite if backend expects /api
      },
    },
  },
});
