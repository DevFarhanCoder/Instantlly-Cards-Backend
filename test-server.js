const express = require('express');
const app = express();
const port = 3001;

app.get('/test', (req, res) => {
  res.json({ message: 'Test server working!' });
});

const server = app.listen(port, '127.0.0.1', () => {
  console.log(`Test server running on 127.0.0.1:${port}`);
});

// Handle server errors
server.on('error', (err) => {
  console.error('Server error:', err);
});