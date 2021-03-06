const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

var babelOptions = {
    presets: [
        ["@babel/preset-env", {
            "targets": {
                "browsers": ["last 2 versions"]
            },
            "modules": false,
            "useBuiltIns": "usage",
            "corejs": 3
        }]
    ],
};

var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: './index.html',
        template: './index.html'
    })
];

module.exports = (env, options) => {
    // If no mode has been defined, default to `development`
    if (options.mode === undefined)
        options.mode = "development";

    var isProduction = options.mode === "production";
    console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

    return {
        devtool: 'inline-source-map',
        entry: isProduction ? // We don't use the same entry for dev and production, to make HMR over style quicker for dev env
            {
                demo: [
                    "@babel/polyfill",
                    './SasTokenParser.fsproj',
                    './main.scss'
                ]
            } : {
                app: [
                    "@babel/polyfill",
                    './SasTokenParser.fsproj'
                ],
                style: [
                    './main.scss'
                ]
            },
        output: {
            path: path.join(__dirname, './bin/publish'),
            filename: isProduction ? '[name].[hash].js' : '[name].js'
        },
        plugins: isProduction ?
            commonPlugins.concat([
                new MiniCssExtractPlugin({
                    filename: 'style.css'
                })
            ])
            : commonPlugins.concat([
                new webpack.HotModuleReplacementPlugin(),
                new webpack.NamedModulesPlugin()
            ]),
        devServer: {
            contentBase: './',
            publicPath: "/",
            port: 8080,
            hot: true,
            inline: true
        },
        module: {
            rules: [
                {
                    test: /\.fs(x|proj)?$/,
                    use: {
                        loader: "fable-loader",
                        options: {
                            babel: babelOptions
                        }
                    }
                },
                {
                    test: /\.js$/,
                    exclude: /node_modules/,
                    use: {
                        loader: 'babel-loader',
                        options: babelOptions
                    },
                },
                {
                    test: /\.(sass|scss|css)$/,
                    use: [
                        isProduction
                            ? MiniCssExtractPlugin.loader
                            : 'style-loader',
                        'css-loader',
                        'sass-loader',
                    ],
                },
                {
                    test: /\.css$/,
                    use: ['style-loader', 'css-loader']
                },
                {
                    test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*$|$)/,
                    use: ["file-loader"]
                }
            ]
        }
    };
}
