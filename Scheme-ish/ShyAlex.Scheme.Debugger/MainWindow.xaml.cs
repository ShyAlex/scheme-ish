using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace ShyAlex.Scheme.Debugger
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            var files = Directory.GetFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*.ss");
            var viewModel = new ViewModel.TreeClientApp(files);
            DataContext = viewModel;
            var i = 0;

            Loaded += (o, e) =>
            {
                var tree = GetChildOfType<WPFExtensions.Controls.ZoomControl>(this);
                viewModel.SteppingForward += () => SaveFrame(tree, String.Format(@"f:\{0}.png", (++i).ToString().PadLeft(5, '0')));
            };
        }

        private void SaveFrame(UIElement obj, String file)
        {
            if (obj.RenderSize.Height <= 0 || obj.RenderSize.Width <= 0)
            {
                return;
            }
            
            var bmp = new RenderTargetBitmap((int)obj.RenderSize.Width, (int)obj.RenderSize.Height, 96, 96, PixelFormats.Pbgra32);
            bmp.Render(obj);
            var e = new PngBitmapEncoder();
            e.Frames.Add(BitmapFrame.Create(bmp));
            
            using (var s = File.OpenWrite(file))
            {
                e.Save(s);
            }
        }

        private static T GetChildOfType<T>(DependencyObject depObj) where T : DependencyObject
        {
            if (depObj == null)
            {
                return null;
            }

            for (int i = 0; i < VisualTreeHelper.GetChildrenCount(depObj); i++)
            {
                var child = VisualTreeHelper.GetChild(depObj, i);
                var result = (child as T) ?? GetChildOfType<T>(child);

                if (result != null)
                {
                    return result;
                }
            }

            return null;
        }
    }
}
