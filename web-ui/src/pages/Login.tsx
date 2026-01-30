import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { setApiToken } from '../api/client';

const Login: React.FC = () => {
  const [apiKey, setApiKey] = useState('');
  const [error, setError] = useState('');
  const navigate = useNavigate();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!apiKey.trim()) {
      setError('Please enter an API key');
      return;
    }

    // Store the API key using the helper function
    setApiToken(apiKey);

    // Redirect to dashboard
    navigate('/');
  };

  return (
    <div style={{
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
      minHeight: '100vh',
      backgroundColor: '#1a1a1a'
    }}>
      <div style={{
        backgroundColor: '#2d2d2d',
        padding: '2rem',
        borderRadius: '8px',
        width: '100%',
        maxWidth: '400px',
        boxShadow: '0 4px 6px rgba(0, 0, 0, 0.3)'
      }}>
        <h1 style={{
          textAlign: 'center',
          marginBottom: '1.5rem',
          color: '#ffffff'
        }}>
          slftp Login
        </h1>

        <form onSubmit={handleSubmit}>
          <div style={{ marginBottom: '1rem' }}>
            <label
              htmlFor="apiKey"
              style={{
                display: 'block',
                marginBottom: '0.5rem',
                color: '#cccccc'
              }}
            >
              API Key
            </label>
            <input
              id="apiKey"
              type="password"
              value={apiKey}
              onChange={(e) => {
                setApiKey(e.target.value);
                setError('');
              }}
              placeholder="Enter your API key"
              style={{
                width: '100%',
                padding: '0.75rem',
                backgroundColor: '#1a1a1a',
                border: '1px solid #444',
                borderRadius: '4px',
                color: '#ffffff',
                fontSize: '1rem'
              }}
            />
          </div>

          {error && (
            <div style={{
              color: '#ff6b6b',
              marginBottom: '1rem',
              fontSize: '0.875rem'
            }}>
              {error}
            </div>
          )}

          <button
            type="submit"
            style={{
              width: '100%',
              padding: '0.75rem',
              backgroundColor: '#4a90e2',
              color: '#ffffff',
              border: 'none',
              borderRadius: '4px',
              fontSize: '1rem',
              cursor: 'pointer',
              fontWeight: 'bold'
            }}
            onMouseOver={(e) => e.currentTarget.style.backgroundColor = '#357abd'}
            onMouseOut={(e) => e.currentTarget.style.backgroundColor = '#4a90e2'}
          >
            Login
          </button>
        </form>

        <div style={{
          marginTop: '1.5rem',
          padding: '1rem',
          backgroundColor: '#1a1a1a',
          borderRadius: '4px',
          fontSize: '0.875rem',
          color: '#888888'
        }}>
          <p style={{ margin: 0 }}>
            <strong style={{ color: '#cccccc' }}>Note:</strong> The API key can be found in your slftp.ini configuration file under [api] section.
          </p>
        </div>
      </div>
    </div>
  );
};

export default Login;
